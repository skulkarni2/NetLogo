// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.nlogo.api.{ AgentException, Color, CompilerServices, ImporterUser,
  LogoException, MersenneTwisterFast, RandomSeedGenerator,
  Timer, TrailDrawerInterface, ValueConstraint, Version, WorldDimensionException }

import org.nlogo.core.{ AgentKind, Breed, Dialect, Nobody, Program,
  Shape, ShapeList, ShapeListTracker, WorldDimensions }

import java.lang.{ Double => JDouble, Integer => JInteger }

import java.util.{ ArrayList => JArrayList, Arrays,
  HashMap => JHashMap, Iterator => JIterator, List => JList,
  Map => JMap }

import java.util.concurrent.CopyOnWriteArrayList

import org.nlogo.agent.Importer.{ ErrorHandler => ImporterErrorHandler, StringReader => ImporterStringReader }

import scala.collection.{ Iterator => SIterator }

object World {
  val Zero = JDouble.valueOf(0.0)
  val One  = JDouble.valueOf(1.0)

  val NegativeOneInt = JInteger.valueOf(-1)

  trait VariableWatcher {
    /**
     * Called when the watched variable is set.
     * @param agent The agent for which the variable was set
     * @param variableName The name of the variable as an upper case string
     * @param value The new value of the variable
     */
    def update(agent: Agent, variableName: String, value: Object): Unit
  }
}

import World._

trait WorldKernel {
  def clearAll(): Unit = {}
  def program: Program
  def observer: Observer
  def observers: AgentSet
  def patches: IndexedAgentSet
  def turtles: TreeAgentSet
  def links: TreeAgentSet
  private[agent] def topology: Topology
  private[agent] def breeds: JMap[String, AgentSet]
  private[agent] def linkBreeds: JMap[String, AgentSet]
}

trait CoreWorld
  extends org.nlogo.api.WorldWithWorldRenderable
  with WorldKernel
  with DimensionManagement {

    // anything that affects the outcome of the model should happen on the
    // main RNG
    val mainRNG: MersenneTwisterFast = new MersenneTwisterFast()

    // anything that doesn't and can happen non-deterministically (for example monitor updates)
    // should happen on the auxillary rng. JobOwners should know which RNG they use.
    val auxRNG: MersenneTwisterFast = new MersenneTwisterFast()

    /// random seed generator
    def generateSeed = RandomSeedGenerator.generateSeed()

    val tieManager: TieManager

    val tickCounter: TickCounter = new TickCounter()

    val timer: Timer = new Timer()

    private[agent] def breeds: JMap[String, AgentSet]
    private[agent] def linkBreeds: JMap[String, AgentSet]

    def linkManager: LinkManager
    // Patches are indexed in row-major order. See `getPatchAt`.
    // This is also true in 3D (with the x-coordinate corresponding to a stride
    // of 1, the y-coordinate with a stride of world-width, and the z-coordinate
    // with a stride of world-width * world-height)
    protected var _patches: IndexedAgentSet = null
    def patches: IndexedAgentSet = _patches

    protected var _links: TreeAgentSet = null;
    def links: TreeAgentSet = _links

    private[agent] var topology: Topology = _

    def getLinkVariablesArraySize(breed: AgentSet): Int

    def getBreedSingular(breed: AgentSet): String
    def getLinkBreedSingular(breed: AgentSet): String

    abstract override def clearAll(): Unit = {
      super.clearAll()
      tickCounter.clear()
    }

    def ticks: Double = tickCounter.ticks

    def allStoredValues: scala.collection.Iterator[Object] = AllStoredValues.apply(this)
}

trait AgentManagement
  extends TurtleManagement
  with LinkManagement
  with ObserverManagement
  with WorldKernel { this: CoreWorld =>

  def program: Program
  def patches: IndexedAgentSet
  def turtles: TreeAgentSet
  def links: TreeAgentSet

  // TODO: Consider whether these can become static vals
  val noTurtles: AgentSet = AgentSet.emptyTurtleSet
  val noPatches: AgentSet = AgentSet.emptyPatchSet
  val noLinks:   AgentSet = AgentSet.emptyLinkSet

  def patchesOwnNameAt(index: Int): String = program.patchesOwn(index)
  def patchesOwnIndexOf(name: String): Int = program.patchesOwn.indexOf(name)
  def observerOwnsNameAt(index: Int): String = program.globals(index)
  def observerOwnsIndexOf(name: String): Int =
    observer.variableIndex(name.toUpperCase)

  protected var breedsOwnCache: JHashMap[String, Integer] = new JHashMap[String, Integer]()

  def createPatches(minPx: Int, maxPx: Int, minPy: Int, maxPy: Int)
  def getPatchAt(x: Double, y: Double): Patch
  def fastGetPatchAt(xc: Int, yc: Int): Patch
  def getOrCreateTurtle(id: Long): Turtle

  /// creating & clearing
  def createPatches(dim: WorldDimensions): Unit = {
    createPatches(dim.minPxcor, dim.maxPxcor, dim.minPycor, dim.maxPycor)
  }

  def getVariablesArraySize(patch: Patch): Int = program.patchesOwn.size

  def indexOfVariable(agentKind: AgentKind, name: String): Int = {
    if (agentKind == AgentKind.Observer)
      observerOwnsIndexOf(name)
    else if (agentKind == AgentKind.Turtle)
      turtlesOwnIndexOf(name)
    else if (agentKind == AgentKind.Link)
      linksOwnIndexOf(name)
    else
      patchesOwnIndexOf(name)
  }

  def getPatch(id: Int): Patch =
    _patches.getByIndex(id).asInstanceOf[Patch]

  abstract override def clearAll(): Unit = {
    super.clearAll()
    clearPatches()
    clearGlobals()
    // Note: for mystery reasons, this used to live at the end of clearTurtles
    observer.updatePosition()
    observer.resetPerspective()
  }

  def getOrCreateLink(end1: JDouble, end2: JDouble, breed: AgentSet): Link = {
    getOrCreateLink(
      getOrCreateTurtle(end1.longValue),
      getOrCreateTurtle(end2.longValue), breed)
  }

  def getOrCreateLink(end1: Turtle, end2: Turtle, breed: AgentSet): Link = {
    val link = getLink(end1.agentKey, end2.agentKey, breed);
    if (link == null) {
      linkManager.createLink(end1, end2, breed)
    } else {
      link
    }
  }

  def getOrCreateDummyLink(end1: Object, end2: Object, breed: AgentSet): Link = {
    val linkOption =
      if (end1 == Nobody || end2 == Nobody) None
      else
        Option(
          getLink(
            end1.asInstanceOf[Turtle].agentKey,
            end2.asInstanceOf[Turtle].agentKey, breed))

    linkOption.getOrElse(new DummyLink(this, end1, end2, breed))
  }

  def indexOfVariable(agent: Agent, name: String): Int = {
    agent match {
      case observer: Observer => observerOwnsIndexOf(name)
      case turtle: Turtle =>
        val breed = turtle.getBreed
        if (breed == _turtles) turtlesOwnIndexOf(name)
        else {
          val breedIndexOf = breedsOwnIndexOf(breed, name)
          if (breedIndexOf != -1) breedIndexOf
          else                    turtlesOwnIndexOf(name)
        }
      case link: Link =>
        val breed = link.getBreed
        if (breed == _links) linksOwnIndexOf(name)
        else {
          val breedIndexOf = linkBreedsOwnIndexOf(breed, name)
          if (breedIndexOf != -1) breedIndexOf
          else                    linksOwnIndexOf(name)
        }
      case _ => patchesOwnIndexOf(name)
    }
  }


  def clearPatches(): Unit = {
    val iter = patches.iterator
    while(iter.hasNext) {
      val patch = iter.next().asInstanceOf[Patch]
      patch.pcolorDoubleUnchecked(Color.BoxedBlack)
      patch.label("")
      patch.labelColor(Color.BoxedWhite)
      try {
        // TODO: we should consider using Arrays.fill here...
        var j = patch.NUMBER_PREDEFINED_VARS
        while (j < patch.variables.length) {
          patch.setPatchVariable(j, Zero)
          j += 1
        }
      } catch {
        case ex: AgentException => throw new IllegalStateException(ex)
      }
    }
  }

  def setUpShapes(clearOld: Boolean): Unit = {
    turtleBreedShapes.setUpBreedShapes(clearOld, breeds)
    linkBreedShapes.setUpBreedShapes(clearOld, linkBreeds)
  }
}

trait World extends CoreWorld with GrossWorldState with AgentManagement {
  def exportWorld(writer: java.io.PrintWriter, full: Boolean): Unit
  def clearDrawing(): Unit
  def protractor: Protractor
  def diffuse(param: Double, vn: Int): Unit
  def diffuse4(param: Double, vn: Int): Unit
  def stamp(agent: Agent, erase: Boolean): Unit
  def changeTopology(xWrapping: Boolean, yWrapping: Boolean): Unit
}

// A note on wrapping: normally whether x and y coordinates wrap is a
// product of the topology.  But we also have the old "-nowrap" primitives
// that don't wrap regardless of what the topology is.  So that's why many
// methods like distance() and towards() take a boolean argument "wrap";
// it's true for the normal prims, false for the nowrap prims. - ST 5/24/06
class World2D extends World with CompilationManagement {

  val protractor: Protractor = new Protractor(this)

  val linkManager: LinkManager =
    new LinkManagerImpl(this,
      new LinkFactory[World]() {
        def apply(world: World, src: Turtle, dest: Turtle, breed: AgentSet): Link = {
          new Link(world, src, dest, breed)
        }
      })
  val tieManager: TieManager = new TieManager(this, linkManager)

  val inRadiusOrCone: InRadiusOrCone = new InRadiusOrCone(this)

  protected val dimensionVariableNames =
    Seq("MIN-PXCOR", "MAX-PXCOR", "MIN-PYCOR", "MAX-PYCOR", "WORLD-WIDTH", "WORLD-HEIGHT")

  // Variable watching *must* be done on variable name, not number. Numbers
  // can change in the middle of runs if, for instance, the user rearranges
  // the order of declarations in turtles-own and then keeps running.
  //
  // I didn't use SimpleChangeEvent here since I wanted the observers to know
  // what the change actually was.
  // -- BCH (4/1/2014)
  private var variableWatchers: JMap[String, JList[VariableWatcher]] =
    new JHashMap[String, JList[VariableWatcher]]()
  // this boolean is micro-optimization to make notifying watchers as fast as possible
  private var hasWatchers: Boolean = false

  /// observer/turtles/patches

  changeTopology(true, true)

  // create patches in the constructor, it's necessary in case
  // the first model we load is 1x1 since when we do create patches
  // in the model loader we only do the reallocation if the dimensions
  // are different than the stored dimensions.  This doesn't come up
  // often because almost always load the default model first, and there
  // aren't many 1x1 models. ev 2/5/07
  createPatches(_minPxcor, _maxPxcor, _minPycor, _maxPycor);
  setUpShapes(true);

  protected def createObserver(): Observer =
    new Observer(this)

  /// get/set methods for World Topology
  private[agent] def getTopology: Topology = topology

  def changeTopology(xWrapping: Boolean, yWrapping: Boolean): Unit = {
    topology = Topology.getTopology(this, xWrapping, yWrapping)
    if (_patches != null) { // is null during initialization
      val it = _patches.iterator
      while (it.hasNext) {
        it.next().asInstanceOf[Patch].topologyChanged()
      }
    }
  }

  /// export world

  def exportWorld(writer: java.io.PrintWriter, full: Boolean): Unit =
    new Exporter(this, writer).exportWorld(full)

  @throws(classOf[java.io.IOException])
  def importWorld(errorHandler: ImporterErrorHandler, importerUser: ImporterUser,
                          stringReader: ImporterStringReader, reader: java.io.BufferedReader): Unit =
    new Importer(errorHandler, this, importerUser, stringReader).importWorld(reader)



  /// line thickness

  /// equality

  private[nlogo] def drawLine(x0: Double, y0: Double, x1: Double, y1: Double, color: Object, size: Double, mode: String): Unit = {
    trailDrawer.drawLine(x0, y0, x1, y1, color, size, mode)
  }

  @throws(classOf[AgentException])
  @throws(classOf[PatchException])
  def diffuse(param: Double, vn: Int): Unit =
    topology.diffuse(param, vn)

  @throws(classOf[AgentException])
  @throws(classOf[PatchException])
  def diffuse4(param: Double, vn: Int): Unit =
    topology.diffuse4(param, vn)


  def agentKindToAgentSet(agentKind: AgentKind): AgentSet = {
    agentKind match {
      case AgentKind.Turtle => _turtles
      case AgentKind.Patch => _patches
      case AgentKind.Observer => observers
      case AgentKind.Link => _links
    }
  }

  def getDimensions: WorldDimensions =
    new WorldDimensions(_minPxcor, _maxPxcor, _minPycor, _maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY)

  // used by Importer and Parser
  def getOrCreateTurtle(id: Long): Turtle = {
    val turtle = getTurtle(id).asInstanceOf[Turtle]
    if (turtle == null) {
      val newTurtle = new Turtle(this, id)
      nextTurtleIndex(StrictMath.max(nextTurtleIndex, id + 1))
      newTurtle
    } else {
      turtle
    }
  }

  @throws(classOf[AgentException])
  def getPatchAt(x: Double, y: Double): Patch = {
    val xc = roundX(x)
    val yc = roundY(y)
    val id = ((_worldWidth * (_maxPycor - yc)) + xc - _minPxcor)
    getPatch(id)
  }

  // this procedure is the same as calling getPatchAt when the topology is a torus
  // meaning it will override the Topology's wrapping rules and
  def getPatchAtWrap(x: Double, y: Double): Patch = {
    val wrappedX = Topology.wrap(x, _minPxcor - 0.5, _maxPxcor + 0.5);
    val wrappedY = Topology.wrap(y, _minPycor - 0.5, _maxPycor + 0.5);
    val xc =
      if (wrappedX > 0) {
        (wrappedX + 0.5).toInt
      } else {
        val intPart = wrappedX.toInt
        val fractPart = intPart - wrappedX
        if (fractPart > 0.5) intPart - 1 else intPart
      }
    val yc =
      if (wrappedY > 0) {
        (wrappedY + 0.5).toInt
      } else {
        val intPart = wrappedY.toInt
        val fractPart = intPart - wrappedY
        if (fractPart > 0.5) intPart - 1 else intPart
      }
    val patchid = ((_worldWidth * (_maxPycor - yc)) + xc - _minPxcor)
    getPatch(patchid)
  }

  def validPatchCoordinates(xc: Int, yc: Int): Boolean =
    xc >= _minPxcor && xc <= _maxPxcor && yc >= _minPycor && yc <= _maxPycor

  def fastGetPatchAt(xc: Int, yc: Int): Patch =
    getPatch(_worldWidth * (_maxPycor - yc) + xc - _minPxcor)

  def createPatches(minPxcor: Int, maxPxcor: Int,
    minPycor: Int, maxPycor: Int): Unit = {

    _patchScratch = null

    _minPxcor = minPxcor
    _maxPxcor = maxPxcor
    _minPycor = minPycor
    _maxPycor = maxPycor
    _worldWidth = maxPxcor - minPxcor + 1
    _worldHeight = maxPycor - minPycor + 1
    _worldWidthBoxed = JDouble.valueOf(_worldWidth)
    _worldHeightBoxed = JDouble.valueOf(_worldHeight)
    _minPxcorBoxed = JDouble.valueOf(_minPxcor)
    _minPycorBoxed = JDouble.valueOf(_minPycor)
    _maxPxcorBoxed = JDouble.valueOf(_maxPxcor)
    _maxPycorBoxed = JDouble.valueOf(_maxPycor)

    rootsTable = new RootsTable(_worldWidth, _worldHeight)

    recreateAllBreeds()

    if (_turtles != null) {
      _turtles.clear() // so a SimpleChangeEvent is published
    }
    _turtles = new TreeAgentSet(AgentKind.Turtle, "TURTLES")
    if (_links != null) {
      _links.clear() // so a SimpleChangeEvent is published
    }
    _links = new TreeAgentSet(AgentKind.Link, "LINKS")

    val patchArray = new Array[Agent](_worldWidth * _worldHeight)
    _patchColors = new Array[Int](_worldWidth * _worldHeight)
    Arrays.fill(_patchColors, Color.getARGBbyPremodulatedColorNumber(0.0))
    _patchColorsDirty = true

    val numVariables = program.patchesOwn.size

    observer.resetPerspective()

    var i = 0
    var x = minPxcor
    var y = maxPycor
    while (i < _worldWidth * _worldHeight) {
      val patch = new Patch(this, i, x, y, numVariables)
      x += 1
      if (x > maxPxcor) {
        x = minPxcor
        y -= 1
      }
      patchArray(i) = patch
      i += 1
    }
    _patches = new ArrayAgentSet(AgentKind.Patch, "patches", patchArray)
    _patchesWithLabels = 0
    _patchesAllBlack = true
    _mayHavePartiallyTransparentObjects = false
  }

  override def clearAll(): Unit = {
    super.clearAll()
  }

  // in a 2D world the drawing lives in the
  // renderer so the workspace takes care of it.
  def clearDrawing(): Unit = { }

  def stamp(agent: Agent, erase: Boolean): Unit = {
    trailDrawer.stamp(agent, erase)
  }

  /// breeds & shapes

  // null indicates failure
  def checkTurtleShapeName(name: String): String = {
    val lowName = name.toLowerCase()
    if (turtleShapeList.exists(lowName)) lowName
    else                                 null
  }

  // null indicates failure
  def checkLinkShapeName(name: String): String = {
    val lowName = name.toLowerCase();
    if (linkShapeList.exists(lowName)) lowName
    else                               null
  }

  def getLinkShape(name: String): Shape = {
    linkShapeList.shape(name)
  }

  /**
   * A watcher to be notified every time the given variable changes for any agent.
   * @param variableName The variable name to watch as an upper case string; e.g. "XCOR"
   * @param watcher The watcher to notify when the variable changes
   */
  def addWatcher(variableName: String, watcher: VariableWatcher): Unit = {
    if (! variableWatchers.containsKey(variableName)) {
      variableWatchers.put(variableName, new CopyOnWriteArrayList[VariableWatcher]())
    }
    variableWatchers.get(variableName).add(watcher)
    hasWatchers = true
  }

  /**
   * Deletes a variable watcher.
   * @param variableName The watched variable name as an upper case string; e.g. "XCOR"
   * @param watcher The watcher to delete
   */
  def deleteWatcher(variableName: String, watcher: VariableWatcher): Unit = {
    if (variableWatchers.containsKey(variableName)) {
      val watchers = variableWatchers.get(variableName)
      watchers.remove(watcher)
      if (watchers.isEmpty) {
        variableWatchers.remove(variableName)
      }
      if (variableWatchers.isEmpty) {
        hasWatchers = false
      }
    }
  }

  def notifyWatchers(agent: Agent, vn: Int, value: Object): Unit = {
    // This needs to be crazy fast if there are no watchers. Thus, hasWatchers. -- BCH (3/31/2014)
    if (hasWatchers) {
      val variableName = agent.variableName(vn)
      val watchers = variableWatchers.get(variableName)
      if (watchers != null) {
        val iter = watchers.iterator
        while (iter.hasNext) {
          iter.next().asInstanceOf[VariableWatcher].update(agent, variableName, value)
        }
      }
    }
  }
}
