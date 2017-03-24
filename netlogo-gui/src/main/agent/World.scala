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

trait TurtleManagement extends WorldKernel { this: CoreWorld =>
  def program: Program
  protected def breedsOwnCache: JHashMap[String, Integer]

  private var _nextTurtleIndex: Long = 0

  protected var _turtles: TreeAgentSet = null
  def turtles: TreeAgentSet = _turtles

  private[agent] var breeds: JMap[String, AgentSet] = new JHashMap[String, AgentSet]()

  protected val lineThicknesses: JMap[Agent, JDouble] = new JHashMap[Agent, JDouble]()

  val turtleShapes = new ShapeListTracker(AgentKind.Turtle)
  def turtleShapeList: ShapeList = turtleShapes.shapeList

  def turtlesOwnNameAt(index: Int): String = program.turtlesOwn(index)
  def turtlesOwnIndexOf(name: String): Int = program.turtlesOwn.indexOf(name)

  def getBreeds:      JMap[String, _ <: org.nlogo.agent.AgentSet] = breeds
  def getAgentBreeds: JMap[String, AgentSet] = breeds
  def getBreed(breedName: String): AgentSet = breeds.get(breedName)
  def isBreed(breed: AgentSet): Boolean =
    program.breeds.isDefinedAt(breed.printName)
  def breedOwns(breed: AgentSet, name: String): Boolean =
    breed != turtles && breedsOwnIndexOf(breed, name) != -1
  def breedsOwnNameAt(breed: org.nlogo.api.AgentSet, index: Int): String =
    program.breeds(breed.printName).owns(index - program.turtlesOwn.size)
  def breedsOwnIndexOf(breed: AgentSet, name: String): Int =
    breedsOwnCache.getOrDefault(breed.printName + "~" + name, NegativeOneInt).intValue
  def getBreedSingular(breed: AgentSet): String =
    if (breed == turtles) "TURTLE"
    else
      program.breeds.get(breed.printName).map(_.singular).getOrElse("TURTLE")

  def getTurtle(id: Long): Turtle =
    _turtles.getAgent(JDouble.valueOf(id)).asInstanceOf[Turtle]

  def nextTurtleIndex(nextTurtleIndex: Long): Unit = {
    _nextTurtleIndex = nextTurtleIndex
  }

  def nextTurtleIndex: Long = _nextTurtleIndex

  def setLineThickness(agent: Agent, size: Double): Unit = {
    lineThicknesses.put(agent, JDouble.valueOf(size))
  }

  def lineThickness(agent: Agent): Double = {
    val size = lineThicknesses.get(agent)
    if (size != null)
      size.doubleValue()
    else
      0.0
  }

  def removeLineThickness(agent: Agent): Unit =
    lineThicknesses.remove(agent)

  def newTurtleId(): Long = {
    val r = _nextTurtleIndex
    _nextTurtleIndex += 1
    r
  }

  abstract override def clearAll() {
    super.clearAll()
    clearTurtles()
  }

  def clearTurtles(): Unit = {
    if (program.breeds.nonEmpty) {
      var breedIterator = breeds.values.iterator
      while (breedIterator.hasNext) {
        breedIterator.next().asInstanceOf[TreeAgentSet].clear()
      }
    }
    val iter = turtles.iterator
    while (iter.hasNext) {
      val turtle = iter.next().asInstanceOf[Turtle]
      lineThicknesses.remove(turtle)
      linkManager.cleanupTurtle(turtle)
      turtle.id(-1)
    }
    turtles.clear()
    val patchIter = patches.iterator
    while (patchIter.hasNext) {
      patchIter.next().asInstanceOf[Patch].clearTurtles()
    }
    _nextTurtleIndex = 0
  }

  def getVariablesArraySize(turtle: org.nlogo.api.Turtle, breed: org.nlogo.api.AgentSet): Int = {
    if (breed == _turtles) {
      program.turtlesOwn.size
    } else {
      val breedOwns = program.breeds(breed.printName).owns
      program.turtlesOwn.size + breedOwns.size
    }
  }

  def createTurtle(breed: AgentSet): Turtle =
    new Turtle(this, breed, Zero, Zero)

  // c must be in 0-13 range
  // h can be out of range
  def createTurtle(breed: AgentSet, c: Int, h: Int): Turtle = {
    val baby = new Turtle(this, breed, Zero, Zero)
    baby.colorDoubleUnchecked(JDouble.valueOf(5 + 10 * c))
    baby.heading(h)
    baby
  }
}

trait LinkManagement extends WorldKernel {
  def program: Program
  def linkManager: LinkManager
  def turtles: TreeAgentSet
  protected def breedsOwnCache: JHashMap[String, Integer]

  // we assign an unique ID to links, like turtles, except that
  // it's not visible to anyone and it can't affect the outcome of
  // the model. I added it because it greatly complicates hubnet
  // view mirroring to have the only unique identifier be a
  // 3 element list. ev 5/1/08
  private var _nextLinkIndex: Long = 0

  private[agent] var linkBreeds: JMap[String, AgentSet] = new JHashMap[String, AgentSet]()

  val linkShapes = new ShapeListTracker(AgentKind.Link)
  def linkShapeList = linkShapes.shapeList

  def linksOwnIndexOf(name: String): Int = program.linksOwn.indexOf(name)
  def linksOwnNameAt(index: Int): String = program.linksOwn(index)
  def getLinkBreeds:      JMap[String, _ <: org.nlogo.agent.AgentSet] = linkBreeds
  def getLinkAgentBreeds: JMap[String, AgentSet] = linkBreeds
  def getLinkBreed(breedName: String): AgentSet = linkBreeds.get(breedName)
  def isLinkBreed(breed: AgentSet): Boolean =
    program.linkBreeds.isDefinedAt(breed.printName)
  def linkBreedOwns(breed: AgentSet, name: String): Boolean =
    breed != links && linkBreedsOwnIndexOf(breed, name) != -1
  def linkBreedsOwnNameAt(breed: AgentSet, index: Int): String =
    program.linkBreeds(breed.printName).owns(index - program.linksOwn.size)
  def linkBreedsOwnIndexOf(breed: AgentSet, name: String): Int =
    breedsOwnCache.getOrDefault(breed.printName + "~" + name, NegativeOneInt).intValue
  def getLinkBreedSingular(breed: AgentSet): String =
    if (breed == links)
      "LINK"
    else
      program.linkBreeds.get(breed.printName).map(_.singular).getOrElse("LINK")

  def newLinkId(): Long = {
    val r = _nextLinkIndex
    _nextLinkIndex += 1
    r
  }

  abstract override def clearAll() {
    super.clearAll()
    clearLinks()
  }

  def clearLinks(): Unit = {
    if (program.linkBreeds.nonEmpty) {
      val breedIterator = linkBreeds.values.iterator
      while (breedIterator.hasNext) {
        breedIterator.next().asInstanceOf[TreeAgentSet].clear()
      }
    }
    val iter = links.iterator
    while (iter.hasNext) {
      iter.next().asInstanceOf[Link].id = -1
    }
    links.clear()
    _nextLinkIndex = 0
    linkManager.reset()
  }

  def getLink(end1: Object, end2: Object, breed: AgentSet): Link = {
    linkManager.getLink(
      turtles.getAgent(end1).asInstanceOf[Turtle],
      turtles.getAgent(end2).asInstanceOf[Turtle], breed).orNull
  }

  def getVariablesArraySize(link: org.nlogo.api.Link, breed: org.nlogo.api.AgentSet): Int = {
    if (breed == links) {
      program.linksOwn.size
    } else {
      val breedOwns = program.linkBreeds(breed.printName).owns
      program.linksOwn.size + breedOwns.size
    }
  }

  def getLinkVariablesArraySize(breed: AgentSet): Int = {
    if (breed == links) {
      program.linksOwn.size
    } else {
      val breedOwns = program.linkBreeds(breed.printName).owns
      program.linksOwn.size + breedOwns.size
    }
  }
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

  def patchesOwnNameAt(index: Int): String = program.patchesOwn(index)
  def patchesOwnIndexOf(name: String): Int = program.patchesOwn.indexOf(name)
  def observerOwnsNameAt(index: Int): String = program.globals(index)
  def observerOwnsIndexOf(name: String): Int =
    observer.variableIndex(name.toUpperCase)

  protected var breedsOwnCache: JHashMap[String, Integer] = new JHashMap[String, Integer]()

  def createPatches(minPx: Int, maxPx: Int, minPy: Int, maxPy: Int)
  def getOrCreateTurtle(id: Long): Turtle

  /// creating & clearing
  def createPatches(dim: WorldDimensions): Unit = {
    createPatches(dim.minPxcor, dim.maxPxcor, dim.minPycor, dim.maxPycor)
  }

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

  protected def buildBreedCaches(): Unit = {
    breedsOwnCache = new JHashMap[String, Integer](16, 0.5f);

    val breedIter = breeds.values.iterator
    while (breedIter.hasNext) {
      val breed = breedIter.next().asInstanceOf[AgentSet]
      val offset = program.turtlesOwn.size
      for {
        b            <- program.breeds.get(breed.printName)
        (varName, i) <- b.owns.zipWithIndex
      } {
        val key = breed.printName + "~" + varName
        breedsOwnCache.put(key, new Integer(offset + i))
      }
    }

    val linkBreedIter = linkBreeds.values.iterator
    while (linkBreedIter.hasNext) {
      val linkBreed = linkBreedIter.next().asInstanceOf[AgentSet]
      val offset = program.linksOwn.size
      for {
        b            <- program.linkBreeds.get(linkBreed.printName)
        (varName, i) <- b.owns.zipWithIndex
      } {
        val key = linkBreed.printName + "~" + varName
        breedsOwnCache.put(key, new Integer(offset + i))
      }
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
}

// A note on wrapping: normally whether x and y coordinates wrap is a
// product of the topology.  But we also have the old "-nowrap" primitives
// that don't wrap regardless of what the topology is.  So that's why many
// methods like distance() and towards() take a boolean argument "wrap";
// it's true for the normal prims, false for the nowrap prims. - ST 5/24/06
class World extends CoreWorld with AgentManagement with GrossWorldState {

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

  private var _compiler: CompilerServices = null
  private var _program: Program = newProgram

  // These are used to cache old values while recompiling...
  private var _oldProgram: Program = null
  private var oldBreeds: JMap[String, AgentSet] = new JHashMap[String, AgentSet]()
  private var oldLinkBreeds: JMap[String, AgentSet] = new JHashMap[String, AgentSet]()

  // This is a flag that the engine checks in its tightest innermost loops
  // to see if maybe it should stop running NetLogo code for a moment
  // and do something like halt or update the display.  It doesn't
  // particularly make sense to keep it in World, but since the check
  // occurs in inner loops, we want to put in a place where the engine
  // can get to it very quickly.  And since every Instruction has a
  // World object in it, the engine can always get to World quickly.
  //  - ST 1/10/07
  @volatile
  var comeUpForAir: Boolean = false  // NOPMD pmd doesn't like 'volatile'

  private var _displayOn: Boolean = true


  val linkBreedShapes = new BreedShapes("LINKS", linkShapes)
  val turtleBreedShapes = new BreedShapes("TURTLES", turtleShapes)

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

  /// empty agentsets

  // TODO: Consider whether these can become static vals
  val noTurtles: AgentSet = AgentSet.emptyTurtleSet
  val noPatches: AgentSet = AgentSet.emptyPatchSet
  val noLinks:   AgentSet = AgentSet.emptyLinkSet

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


  /// random seed generator

  def generateSeed = RandomSeedGenerator.generateSeed()

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

  private def createBreeds(
      programBreeds: scala.collection.Map[String, Breed],
      worldBreeds: JMap[String, AgentSet]): Unit = {

    programBreeds.foreach {
      case (name: String, breed: Breed) =>
        val agentKind = if (breed.isLinkBreed) AgentKind.Link else AgentKind.Turtle
        val agentset = new TreeAgentSet(agentKind, breed.name)
        if (breed.isLinkBreed) {
          agentset.setDirected(breed.isDirected)
        }
        worldBreeds.put(name.toUpperCase, agentset)
    }
  }

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

    breeds.clear()
    createBreeds(_program.breeds, breeds)

    linkBreeds.clear()
    createBreeds(_program.linkBreeds, linkBreeds)

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

    val numVariables = _program.patchesOwn.size

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

  // this exists to support recompiling a model without causing
  // agent state information to be lost.  it is called after a
  // successful recompilation.
  def realloc(): Unit = {
    // copy the breed agentsets from the old Program object from
    // the previous compile to the new Program object that was
    // created when we recompiled.  any new breeds that were created,
    // we create new agentsets for.  (if this is a first compile, all
    // the breeds will be created.)  any breeds that no longer exist
    // are dropped.
    if (_program.breeds.nonEmpty) {
      _program.breeds.keys.foreach { breedName =>
        val upcaseName = breedName.toUpperCase
        val breedSet =
          Option(oldBreeds.get(upcaseName)).getOrElse(new TreeAgentSet(AgentKind.Turtle, upcaseName))
        breeds.put(upcaseName, breedSet)
      }
    } else {
      breeds.clear()
    }

    if (_program.linkBreeds.nonEmpty) {
      _program.linkBreeds.foreach {
        case (_, linkBreed) =>
          val breedName = linkBreed.name.toUpperCase
          val directed = linkBreed.isDirected
          val breedSet = Option(oldLinkBreeds.get(breedName)).getOrElse(new TreeAgentSet(AgentKind.Link, breedName))
          breedSet.setDirected(directed)
          linkBreeds.put(breedName, breedSet)
      }
    } else {
      linkBreeds.clear()
    }

    val doomedAgents: JList[Agent] = new JArrayList[Agent]()

    val compiling = _oldProgram != null

    // call Agent.realloc() on all the turtles
    try {
      if (_turtles != null) {
        val iter = _turtles.iterator
        while (iter.hasNext) {
          Option(iter.next().realloc(compiling)).foreach(doomedAgents.add)
        }
        val doomedIter = doomedAgents.iterator
        while (doomedIter.hasNext) {
          doomedIter.next().asInstanceOf[Turtle].die()
        }
        doomedAgents.clear()
      }
    } catch {
      case ex: AgentException => throw new IllegalStateException(ex)
    }
    // call Agent.realloc() on all links
    try {
      if (_links != null) {
        val iter = _links.iterator
        while (iter.hasNext) {
          Option(iter.next().realloc(compiling)).foreach(doomedAgents.add)
        }
        val doomedIter = doomedAgents.iterator
        while (doomedIter.hasNext) {
          doomedIter.next().asInstanceOf[Link].die()
        }
        doomedAgents.clear()
      }
    } catch {
      case ex: AgentException => throw new IllegalStateException(ex)
    }
    // call Agent.realloc() on all the patches
    try {
      // Note: we only need to realloc() if the patch variables have changed.
      //  ~Forrest ( 5/2/2007)
      if (_patches != null && ((! compiling) || _program.patchesOwn != _oldProgram.patchesOwn)) {
        val iter = _patches.iterator
        while (iter.hasNext) {
          iter.next().realloc(compiling)
        }
      }
    } catch {
      case ex: AgentException => throw new IllegalStateException(ex)
    }
    // call Agent.realloc() on the observer
    observer.realloc(compiling)
    // and finally...
    setUpShapes(false)
    buildBreedCaches()
    _oldProgram = null
  }

  /// agent-owns

  //TODO: We can remove these if we pass _oldProgram to realloc
  def oldTurtlesOwnIndexOf(name: String): Int = _oldProgram.turtlesOwn.indexOf(name)
  def oldLinksOwnIndexOf(name: String): Int = _oldProgram.linksOwn.indexOf(name)

  /**
   * used by Turtle.realloc()
   */
  def oldBreedsOwnIndexOf(breed: AgentSet, name: String): Int = {
    _oldProgram.breeds.get(breed.printName)
      .map(b => b.owns.indexOf(name))
      .filter(_ != -1)
      .map(i => _oldProgram.turtlesOwn.size + i)
      .getOrElse(-1)
  }

  /**
   * used by Link.realloc()
   */
  def oldLinkBreedsOwnIndexOf(breed: AgentSet, name: String): Int = {
    _oldProgram.linkBreeds.get(breed.printName)
      .map(b => b.owns.indexOf(name))
      .filter(_ != -1)
      .map(i => _oldProgram.linksOwn.size + i)
      .getOrElse(-1)
  }


  /// breeds & shapes

  // assumes caller has already checked to see if the breeds are equal
  def compareLinkBreeds(breed1: AgentSet, breed2: AgentSet): Int = {
    val iter = linkBreeds.values.iterator
    while (iter.hasNext) {
      val next = iter.next()
      if (next == breed1) {
        return -1
      } else {
        return 1
      }
    }

    throw new IllegalStateException("neither of the breeds exist, that's bad");
  }

  def getVariablesArraySize(patch: Patch): Int = _program.patchesOwn.size

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

  /// program

  def program: Program = _program
  def oldProgram: Program = _oldProgram

  def program(program: Program): Unit = {
    if (program == null) {
      throw new IllegalArgumentException("World.program cannot be set to null")
    }
    _program = program

    breeds.clear()
    linkBreeds.clear()
    createBreeds(_program.breeds, breeds)
    createBreeds(_program.linkBreeds, linkBreeds)
  }

  def newProgram: Program = {
    val dialect =
      if (Version.is3D) org.nlogo.api.NetLogoThreeDDialect
      else              org.nlogo.api.NetLogoLegacyDialect

    Program.fromDialect(dialect)
  }

  def newProgram(interfaceGlobals: scala.collection.Seq[String]): Program =
    newProgram.copy(interfaceGlobals = interfaceGlobals)

  def rememberOldProgram(): Unit = {
    _oldProgram = _program
    oldBreeds = new JHashMap[String, AgentSet](breeds)
    oldLinkBreeds = new JHashMap[String, AgentSet](linkBreeds)
  }

  def displayOn = _displayOn

  def displayOn(displayOn: Boolean): Unit = {
    _displayOn = displayOn
  }

  /// accessing observer variables by name;

  def getObserverVariableByName(varName: String): AnyRef = {
    val index = observer.variableIndex(varName.toUpperCase)
    if (index >= 0)
      observer.variables(index)
    else
      throw new IllegalArgumentException(s""""${varName}" not found""")
  }

  def compiler_=(compiler: CompilerServices): Unit = {
    _compiler = compiler
  }

  def compiler: CompilerServices = _compiler

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

  def setUpShapes(clearOld: Boolean): Unit = {
    turtleBreedShapes.setUpBreedShapes(clearOld, breeds)
    linkBreedShapes.setUpBreedShapes(clearOld, linkBreeds)
  }
}
