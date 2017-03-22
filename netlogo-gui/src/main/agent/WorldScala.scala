// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.nlogo.api.{ AgentException, Color, CompilerServices, ImporterUser,
  LogoException, MersenneTwisterFast, RandomSeedGenerator,
  Timer, TrailDrawerInterface, ValueConstraint, Version, WorldDimensionException }

import org.nlogo.core.{ AgentKind, Breed, Dialect, Nobody$, Program,
  Shape, ShapeList, ShapeListTracker, WorldDimensions }

import java.lang.{ Double => JDouble }

import java.util.{ ArrayList => JArrayList, Arrays,
  HashMap => JHashMap, Iterator => JIterator, List => JList,
  Map => JMap }

import java.util.concurrent.CopyOnWriteArrayList

import org.nlogo.agent.Importer.{ ErrorHandler => ImporterErrorHandler, StringReader => ImporterStringReader }

import scala.collection.{ Iterator => SIterator }

object WorldScala {
  val Zero = JDouble.valueOf(0.0)
  val One  = JDouble.valueOf(1.0)
}

import WorldScala._

// A note on wrapping: normally whether x and y coordinates wrap is a
// product of the topology.  But we also have the old "-nowrap" primitives
// that don't wrap regardless of what the topology is.  So that's why many
// methods like distance() and towards() take a boolean argument "wrap";
// it's true for the normal prims, false for the nowrap prims. - ST 5/24/06

public strictfp class WorldScala
  extends org.nlogo.api.World
  with org.nlogo.api.WorldRenderable
  with org.nlogo.api.WorldWithWorldRenderable {

  val tickCounter: TickCounter = new TickCounter()

  def ticks: Double = tickCounter.ticks()

  val timer: Timer = new Timer()

  private val turtleShapes = new ShapeListTracker(AgentKind.Turtle)
  def turtleShapeList: ShapeList = turtleShapes.shapeList()

  private val linkShapes = new ShapeListTracker(AgentKind.Link)
  def linkShapeList = linkShapes.shapeList()

  private val lineThicknesses: JMap[Agent, JDouble] = new JHashMap[Agent, JDouble]()
  private var _patchSize: Double = 12.0
  private var _trailDrawer: TrailDrawerInterface = _

  private var _topology: Topology = _
  private var rootsTable: RootsTable = _

  val protractor: Protractor = new Protractor(this)

  val linkManager: LinkManager =
    new LinkManagerImpl(this,
      new LinkFactory() {
        def apply(world: World, src: Turtle, dest: Turtle, breed: AgentSet): Link = {
          new Link(world, src, dest, breed)
        }
      })
  val tieManager: TieManager = new TieManager(this, linkManager)

  val inRadiusOrCone: InRadiusOrCone = new InRadiusOrCone(this)

  private var JMap[String, AgentSet] breeds     = new JHashMap[String, AgentSet]()
  private var JMap[String, AgentSet] linkBreeds = new JHashMap[String, AgentSet]()

  private var Long _nextTurtleIndex = 0

  // we assign an unique ID to links, like turtles, except that
  // it's not visible to anyone and it can't affect the outcome of
  // the model. I added it because it greatly complicates hubnet
  // view mirroring to have the only unique identifier be a
  // 3 element list. ev 5/1/08
  private var Long _nextLinkIndex = 0

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

  // possibly need another array for 3D colors
  // since it seems messy to collapse 3D array into 2D
  private var _patchColors: Array[Int]

  // GLView
  // this is used by the OpenGL texture code to decide whether
  // it needs to make a new texture or not - ST 2/9/05
  private var _patchColorsDirty: Boolean = true

  // performance optimization -- avoid drawing an all-black bitmap if we
  // could just paint one big black rectangle
  private var _patchesAllBlack = true

  // for efficiency in Renderer
  private var _patchesWithLabels: Int = 0

  // performance optimization for 3D renderer -- avoid sorting by distance
  // from observer unless we need to.  once this flag becomes true, we don't
  // work as hard as we could to return it back to false, because doing so
  // would be expensive.  we just reset it at clear-all time.
  private var _mayHavePartiallyTransparentObjects = false

  val linkBreedShapes = new BreedShapes("LINKS", linkShapes)
  val turtleBreedShapes = new BreedShapes("TURTLES", _turtleShapes);

  val observer: Observer = createObserver()
  val observers: AgentSet = AgentSet.fromAgent(_observer)

  // anything that affects the outcome of the model should happen on the
  // main RNG
  val mainRNG: MersenneTwisterFast = new MersenneTwisterFast()

  // anything that doesn't and can happen non-deterministically (for example monitor updates)
  // should happen on the auxillary rng. JobOwners should know which RNG they use.
  val auxRNG: MersenneTwisterFast = new MersenneTwisterFast()

  changeTopology(true, true);

  // create patches in the constructor, it's necessary in case
  // the first model we load is 1x1 since when we do create patches
  // in the model loader we only do the reallocation if the dimensions
  // are different than the stored dimensions.  This doesn't come up
  // often because almost always load the default model first, and there
  // aren't many 1x1 models. ev 2/5/07
  createPatches(_minPxcor, _maxPxcor, _minPycor, _maxPycor);
  setUpShapes(true);

  private def createObserver(): Observer =
    new Observer(this)

  /// empty agentsets

  // TODO: Consider whether these can become static vals
  val noTurtles: AgentSet = AgentSet.emptyTurtleSet()
  val noPatches: AgentSet = AgentSet.emptyPatchSet()
  val noLinks:   AgentSet = AgentSet.emptyLinkSet()

  def trailDrawer(trailDrawer: TrailDrawerInterface): Unit = {
    _trailDrawer = trailDrawer
  }

  def trailDrawer = _trailDrawer

  /// get/set methods for World Topology
  private[agent] def getTopology: Topology = _topology

  def changeTopology(xWrapping: Boolean, yWrapping: Boolean): Unit = {
    _topology = Topology.getTopology(this, xWrapping, yWrapping)
    if (_patches != null) { // is null during initialization
      val it = _patches.iterator()
      while (it.hasNext()) {
        it.next().asInstanceOf[Patch].topologyChanged()
      }
    }
  }

  val wrappedObserverX(x: Double): Double = {
    try {
      topology.wrapX(x - topology.followOffsetX)
    } catch {
      case e: AgentException =>
        org.nlogo.api.Exceptions.ignore(e)
        x
    }
  }

  def wrappedObserverY(y: Double): Double = {
    try {
      topology.wrapY(y - topology.followOffsetY)
    } catch {
      case e: AgentException =>
        org.nlogo.api.Exceptions.ignore(e);
        y
    }
  }

  def followOffsetX: Double = observer.followOffsetX
  def followOffsetY: Double = observer.followOffsetY

  // These are just being used for setting the checkboxes in the ViewWidget config dialog
  //   with default values. These should no be used within World to control any behavior.
  //   All wrapping related behavior specific to a topology is/should-be hardcoded in the methods
  //   for each specific topological implementation.
  def wrappingAllowedInX: Boolean =
    topology.isInstanceOf[Torus] || topology.isInstanceOf[VertCylinder]

  def wrappingAllowedInY: Boolean =
    topology.isInstanceOf[Torus] || topology.isInstanceOf[HorizCylinder]

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

  val removeLineThickness(agent: Agent): Unit =
    lineThicknesses.remove(agent)

  /// equality

  private[nlogo] def drawLine(x0: Double, y0: Double, x1: Double, y1: Double, color: Object, size: Double, mode: String): Unit = {
    trailDrawer.drawLine(x0, y0, x1, y1, color, size, mode)
  }

  // boxed versions of geometry/size methods, for efficiency
  var _worldWidthBoxed: JDouble = _
  var _worldHeightBoxed: JDouble = _
  var _minPxcorBoxed: JDouble = _
  var _minPycorBoxed: JDouble = _
  var _maxPxcorBoxed: JDouble = _
  var _maxPycorBoxed: JDouble = _

  def worldWidthBoxed = _worldWidthBoxed
  def worldHeightBoxed = _worldHeightBoxed
  def minPxcorBoxed = _minPxcorBoxed
  def minPycorBoxed = _minPycorBoxed
  def maxPxcorBoxed = _maxPxcorBoxed
  def maxPycorBoxed = _maxPycorBoxed

  /// world geometry

  var _worldWidth: Int = _
  var _worldHeight: Int = _
  var _minPxcor: Int = _
  var _minPycor: Int = _
  var _maxPycor: Int = _
  var _maxPxcor: Int = _

  def worldWidth: Int = _worldWidth
  def worldHeight: Int = _worldHeight
  def minPxcor: Int = _minPxcor
  def minPycor: Int = _minPycor
  def maxPycor: Int = _maxPycor
  def maxPxcor: Int = _maxPxcor

  @throws(classOf[AgentException])
  def wrapX(x: Double): Double = topology.wrapX(x)
  @throws(classOf[AgentException])
  def wrapY(y: Double): Double = topology.wrapY(y)

  def wrap(pos: Double, min: Double, max: Double): Double =
    Topology.wrap(pos, min, max)

  @throws(classOf[AgentException])
  @throws(classOf[PatchException])
  def diffuse(param: Double, vn: Int): Unit =
    topology.diffuse(param, vn)

  @throws(classOf[AgentException])
  @throws(classOf[PatchException])
  def diffuse4(param: Double, vn: Int): Unit =
    topology.diffuse4(param, vn)


  @throws(classOf[AgentException])
  def roundX(x: Double): Int = {
    // floor() is slow so we don't use it
    val wrappedX =
      try {
        topology.wrapX(x)
      } catch (AgentException ex) {
        throw new AgentException("Cannot access patches beyond the limits of current world.");
      }
    if (wrappedX > 0) {
      (wrappedX + 0.5).toInt
    } else {
      val intPart = wrappedX.toInt
      val fractPart = intPart - wrappedX
      if (fractPart > 0.5) intPart - 1 else intPart
    }
  }

  @throws(classOf[AgentException])
  def roundY(y: Double): Int = {
    // floor() is slow so we don't use it
    val wrappedY =
      try {
        topology.wrapY(y);
      } catch (AgentException ex) {
        throw new AgentException("Cannot access patches beyond the limits of current world.");
      }
    if (wrappedY > 0) {
      (wrappedY + 0.5).toInt
    } else {
      val intPart = wrappedY.toInt
      val fractPart = intPart - wrappedY
      if (fractPart > 0.5) intPart - 1 else intPart
    }
  }

  val createTurtle(breed: AgentSet): Turtle =
    new Turtle(this, breed, Zero, Zero)

  // c must be in 0-13 range
  // h can be out of range
  def createTurtle(breed: AgentSet, c: Int, h: Int): Turtle {
    val baby = new Turtle(this, breed, Zero, Zero)
    baby.colorDoubleUnchecked(JDouble.valueOf(5 + 10 * c))
    baby.heading(h)
    baby
  }

  /// observer/turtles/patches

  // Patches are indexed in row-major order. See `getPatchAt`.
  // This is also true in 3D (with the x-coordinate corresponding to a stride
  // of 1, the y-coordinate with a stride of world-width, and the z-coordinate
  // with a stride of world-width * world-height)
  private _patches: IndexedAgentSet = null
  def patches: IndexedAgentSet = _patches

  private var _turtles: TreeAgentSet = null
  def turtles: TreeAgentSet = _turtles

  private var _links: TreeAgentSet = null;
  def links: TreeAgentSet = _links

  def agentKindToAgentSet(agentKind: AgentKind): AgentSet = {
    agentKind match {
      case AgentKind.Turtle => _turtles
      case AgentKind.Patch => _patches
      case AgentKind.Observer => _observers
      case AgentKind.Link => _links
    }
  }

  def getDimensions: WorldDimensions =
    new WorldDimensions(_minPxcor, _maxPxcor, _minPycor, _maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY)

  def isDimensionVariable(variableName: String): Boolean = {
    variableName.equalsIgnoreCase("MIN-PXCOR") ||
    variableName.equalsIgnoreCase("MAX-PXCOR") ||
    variableName.equalsIgnoreCase("MIN-PYCOR") ||
    variableName.equalsIgnoreCase("MAX-PYCOR") ||
    variableName.equalsIgnoreCase("WORLD-WIDTH") ||
    variableName.equalsIgnoreCase("WORLD-HEIGHT")
  }

  @throws(classOf[WorldDimensionException])
  def setDimensionVariable(variableName: String, value: Int, d: WorldDimensions): WorldDimensions = {
    if (variableName.equalsIgnoreCase("MIN-PXCOR")) {
      new WorldDimensions(value,      d.maxPxcor, d.minPycor, d.maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY);
    } else if (variableName.equalsIgnoreCase("MAX-PXCOR")) {
      new WorldDimensions(d.minPxcor, value,      d.minPycor, d.maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY);
    } else if (variableName.equalsIgnoreCase("MIN-PYCOR")) {
      new WorldDimensions(d.minPxcor, d.maxPxcor, value,      d.maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY);
    } else if (variableName.equalsIgnoreCase("MAX-PYCOR")) {
      new WorldDimensions(d.minPxcor, d.maxPxcor, d.minPycor, value,     patchSize, wrappingAllowedInX, wrappingAllowedInY);
    } else if (variableName.equalsIgnoreCase("WORLD-WIDTH")) {
      val minPxcor = growMin(d.minPxcor, d.maxPxcor, value, d.minPxcor)
      val maxPxcor = growMax(d.minPxcor, d.maxPxcor, value, d.maxPxcor)
      new WorldDimensions(minPxcor, maxPxcor, d.minPycor, d.maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY);
    } else if (variableName.equalsIgnoreCase("WORLD-HEIGHT")) {
      val minPycor = growMin(d.minPycor, d.maxPycor, value, d.minPycor)
      val maxPycor = growMax(d.minPycor, d.maxPycor, value, d.maxPycor)
      new WorldDimensions(d.minPxcor, d.maxPxcor, minPycor, maxPycor, patchSize, wrappingAllowedInX, wrappingAllowedInY);
    } else {
      d
    }
  }

  @throws(classOf[WorldDimensionException])
  def growMin(min: Int, max: Int, value: Int, d: Int): Int = {
    if (value < 1) {
      throw new WorldDimensionException()
    }

    if (max == -min) {
      if (value % 2 != 1) {
        throw new WorldDimensionException()
      }
      -(value - 1) / 2
    } else if (max == 0)
      -(value - 1)
    else
      return d
  }

  @throws(classOf[WorldDimensionException])
  def growMax(min: Int, max: Int, value: Int, d: Int): Int = {
    if (value < 1) {
      throw new WorldDimensionException()
    }

    if (max == -min) {
      if (value % 2 != 1) {
        throw new WorldDimensionException()
      }
      (value - 1) / 2
    } else if (min == 0)
      (value - 1)
    else
      d
  }

  def equalDimensions(d: WorldDimensions): Boolean =
    d.minPxcor == _minPxcor &&
      d.maxPxcor == _maxPxcor &&
      d.minPycor == _minPycor &&
      d.maxPycor == _maxPycor

  def getPatch(id: Int): Patch =
    _patches.getByIndex(id).asInstanceOf[Patch]

  @throws(classOf[AgentException])
  def getPatchAt(Double x, Double y): Patch = {
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
        val intPart = (int) wrappedY
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

  def getTurtle(id: Long): Turtle =
    _turtles.getAgent(JDouble.valueOf(id)).asInstanceOf[Turtle]

  def getLink(end1: Object, end2: Object, breed: AgentSet): Link  = {
    linkManager.getLink(
      _turtles.getAgent(end1).asInstanceOf[Turtle],
      _turtles.getAgent(end2).asInstanceOf[Turtle], breed).orNull
  }

  def nextTurtleIndex(nextTurtleIndex: Long): Unit = {
    _nextTurtleIndex = nextTurtleIndex
  }

  def nextTurtleIndex: Long = _nextTurtleIndex

  def newTurtleId(): Long = {
    val r = nextTurtleIndex
    nextTurtleIndex += 1
    r
  }

  def newLinkId(): Long = {
    val r = nextLinkIndex
    nextLinkIndex += 1
    r
  }

  // used by Importer and Parser
  def getOrCreateTurtle(id: Long): Turtle = {
    val turtle = getTurtle(id).asInstanceOf[Turtle]
    if (turtle == null) {
      val newTurtle = new Turtle(this, id)
      nextTurtleIndex = StrictMath.max(nextTurtleIndex, id + 1)
      newTurtle
    } else {
      turtle
    }
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

    link.getOrElse(new DummyLink(this, end1, end2, breed))
  }

  def patchColorsDirty: Boolean = _patchColorsDirty

  def markPatchColorsDirty(): Unit = { _patchColorsDirty = true }

  def markPatchColorsClean(): Unit = { _patchColorsDirty = false }

  def patchesAllBlack: Boolean = _patchesAllBlack

  def mayHavePartiallyTransparentObjects: Boolean = _mayHavePartiallyTransparentObjects

  def patchColors: Array[Int] = _patchColors

  def patchesWithLabels: Int = patchesWithLabels

  /// creating & clearing
  def createPatches(dim: WorldDimensions): Unit = {
    createPatches(dim.minPxcor, dim.maxPxcor, dim.minPycor, dim.maxPycor)
  }

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

    patchScratch = null

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
    _turtles = new TreeAgentSet(AgentKindJ.Turtle(), "TURTLES")
    if (_links != null) {
      _links.clear() // so a SimpleChangeEvent is published
    }
    _links = new TreeAgentSet(AgentKindJ.Link(), "LINKS")

    var x = minPxcor
    var y = maxPycor
    val patchArray = new Array[Agent](_worldWidth * _worldHeight)
    _patchColors = new Array[Int](_worldWidth * _worldHeight)
    Arrays.fill(_patchColors, Color.getARGBbyPremodulatedColorNumber(0.0))
    _patchColorsDirty = true

    val numVariables = _program.patchesOwn.size;

    _observer.resetPerspective

    var i = 0
    while (i < _worldHeight * _worldHeight) {
      val patch = new Patch(this, i, x, y, numVariables)
      x += 1
      if (x > maxPxcor) {
        x = minPxcor
        y -= 1
      }
      patchArray(i) = patch;
      i += 1
    }
    _patches = new ArrayAgentSet(AgentKindJ.Patch(), "patches", patchArray)
    _patchesWithLabels = 0
    _patchesAllBlack = true
    _mayHavePartiallyTransparentObjects = false
  }

  def clearAll(): Unit = {
    tickCounter.clear()
    clearTurtles()
    clearPatches()
    clearGlobals()
    clearLinks()
    _observer.resetPerspective()
    mayHavePartiallyTransparentObjects = false
  }

  // in a 2D world the drawing lives in the
  // renderer so the workspace takes care of it.
  def clearDrawing(): Unit = { }

  def stamp(agent: Agent, erase: Boolean): Unit = {
    trailDrawer.stamp(agent, erase)
  }

  def patchSize: Double = _patchSize

  def patchSize(patchSize: Double): Boolean =
    if (_patchSize != patchSize) {
      _patchSize = patchSize
      true
    } else {
      false
    }

  def getDrawing: AnyRef = _trailDrawer.getDrawing()

  def sendPixels(): Boolean = _trailDrawer.sendPixels()

  def markDrawingClean(): Unit = {
    _trailDrawer.sendPixels(false)
  }

  def clearPatches(): Unit = {
    val iter = _patches.iterator()
    while(iter.hasNext()) {
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
    patchesAllBlack = true;
  }

  def clearTurtles(): Unit = {
    if (_program.breeds.nonEmpty) {
      var breedIterator = breeds.values.iterator
      while (breedIterator.hasNext) {
        breedIterator.next().asInstanceOf[TreeAgentSet].clear()
      }
    }
    val iter = _turtles.iterator
    while (iter.hasNext) {
      val turtle = iter.next().asInstanceOf[Turtle]
      lineThicknesses.remove(turtle)
      linkManager.clearTurtle(turtle)
      turtle.id(-1)
    }
    _turtles.clear()
    val patchIter = _patches.iterator()
    while (patchIter.hasNext) {
      iter.next().asInstanceOf[Patch].clearTurtles()
    }
    _nextTurtleIndex = 0
    _observer.updatePosition()
  }

  def clearLinks(): Unit = {
    if (_program.linkBreeds.nonEmpty) {
      val breedIterator = linkBreeds.values.iterator
      while (breedIterator.hasNext) {
        breedIterator.next().asInstanceOf[TreeAgentSet].clear()
      }
    }
    val iter = _links.iterator
    while (iter.hasNext) {
      iter.next().asInstanceOf[Link].id = -1
    }
    _links.clear()
    _nextLinkIndex = 0
    linkManager.reset()
  }

  def clearGlobals(): Unit = {
    var j = _program.interfaceGlobals.size
    while (j < _observer.variables.length) {
      try {
        val con = Option(_observer.variableConstraint(j))
        _observer.setObserverVariable(j, con.map(_.defaultValue()).getOrElse(Zero))
      } catch {
        case ex: AgentException => throw new IllegalStateException(ex)
        case ex: LogoException  => throw new IllegalStateException(ex)
      }
      j += 1
    }
  }

  // this exists to support recompiling a model without causing
  // agent state information to be lost.  it is called after a
  // successful recompilation.
  public void realloc() {
    // copy the breed agentsets from the old Program object from
    // the previous compile to the new Program object that was
    // created when we recompiled.  any new breeds that were created,
    // we create new agentsets for.  (if this is a first compile, all
    // the breeds will be created.)  any breeds that no longer exist
    // are dropped.
    scala.collection.Iterator[String] breedNameIterator = _program.breeds().keysIterator();
    if (breedNameIterator.hasNext()) {
      while (breedNameIterator.hasNext()) {
        String breedName = breedNameIterator.next().toUpperCase();
        AgentSet breedSet = oldBreeds.get(breedName);
        if (breedSet == null) {
          breeds.put(breedName, new TreeAgentSet(AgentKindJ.Turtle(), breedName));
        } else {
          breeds.put(breedName, breedSet);
        }
      }
    } else {
      breeds.clear();
    }

    scala.collection.Iterator[Breed] linkBreedIterator = _program.linkBreeds().values().iterator();
    if (linkBreedIterator.hasNext()) {
      while (linkBreedIterator.hasNext()) {
        Breed breed = linkBreedIterator.next();
        String breedName  = breed.name().toUpperCase();
        boolean directed  = breed.isDirected();
        AgentSet breedSet = oldLinkBreeds.get(breedName);
        if (breedSet == null) {
          breedSet = new TreeAgentSet(AgentKindJ.Link(), breedName);
        } else {
          breedSet.clearDirected();
        }
        linkBreeds.put(breedName, breedSet);
        breedSet.setDirected(directed);
      }
    } else {
      linkBreeds.clear();
    }

    JList[Agent] doomedAgents = new JArrayList[Agent]();
    // call Agent.realloc() on all the turtles
    try {
      if (_turtles != null) {
        for (AgentIterator iter = _turtles.iterator(); iter.hasNext();) {
          Agent agt = iter.next().realloc(_oldProgram != null);
          if (agt != null) {
            doomedAgents.add(agt);
          }
        }
        for (JIterator[Agent] i = doomedAgents.iterator(); i.hasNext();) {
          ((Turtle) i.next()).die();
        }
        doomedAgents.clear();
      }
    } catch (AgentException ex) {
      throw new IllegalStateException(ex);
    }
    // call Agent.realloc() on all links
    try {
      if (_links != null) {
        for (AgentIterator iter = _links.iterator(); iter.hasNext();) {
          Agent agt = iter.next().realloc(_oldProgram != null);
          if (agt != null) {
            doomedAgents.add(agt);
          }
        }
        for (JIterator[Agent] i = doomedAgents.iterator(); i.hasNext();) {
          ((Link) i.next()).die();
        }
        doomedAgents.clear();
      }
    } catch (AgentException ex) {
      throw new IllegalStateException(ex);
    }
    // call Agent.realloc() on all the patches
    try {
      // Note: we only need to realloc() if the patch variables have changed.
      //  ~Forrest ( 5/2/2007)
      if (_patches != null &&
          (_oldProgram == null || !_program.patchesOwn().equals(_oldProgram.patchesOwn()))) {
        for (AgentIterator iter = _patches.iterator(); iter.hasNext();) {
          iter.next().realloc(_oldProgram != null);
        }
      }
    } catch (AgentException ex) {
      throw new IllegalStateException(ex);
    }
    // call Agent.realloc() on the observer
    _observer.realloc(_oldProgram != null);
    // and finally...
    setUpShapes(false);
    buildBreedCaches();
    _oldProgram = null;
  }

  private JHashMap[String, Integer] breedsOwnCache = new JHashMap[String, Integer]();

  private void buildBreedCaches() {
    breedsOwnCache = new JHashMap[String, Integer](16, 0.5f);
    for (AgentSet breed : breeds.values()) {
      scala.Option[Breed] b = _program.breeds().get(breed.printName());
      if (! b.isEmpty()) {
        scala.collection.Iterator[String] ownsIter = b.get().owns().iterator();
        int offset = _program.turtlesOwn().size();
        while (ownsIter.hasNext()) {
          String varName = ownsIter.next();
          String key = breed.printName() + "~" + varName;
          breedsOwnCache.put(key, new Integer(offset));
          offset++;
        }
      }
    }

    for (AgentSet linkBreed: linkBreeds.values()) {
      scala.Option[Breed] b = _program.linkBreeds().get(linkBreed.printName());
      if (! b.isEmpty()) {
        scala.collection.Iterator[String] ownsIter = b.get().owns().iterator();
        int offset = _program.linksOwn().size();
        while(ownsIter.hasNext()) {
          String varName = ownsIter.next();
          String key = linkBreed.printName() + "~" + varName;
          breedsOwnCache.put(key, new Integer(offset));
          offset++;
        }
      }
    }
  }

  /// patch scratch
  //  a scratch area that can be used by commands such as _diffuse

  Array[Array[Double]] patchScratch;

  public Array[Array[Double]] getPatchScratch() {
    if (patchScratch == null) {
      patchScratch = new Double[_worldWidth][_worldHeight];
    }
    return patchScratch;
  }

  /// agent-owns

  public int indexOfVariable(AgentKind agentKind, String name) {
    if (agentKind == AgentKindJ.Observer()) {
      return observerOwnsIndexOf(name);
    } else if (agentKind == AgentKindJ.Turtle()) {
      return turtlesOwnIndexOf(name);
    } else if (agentKind == AgentKindJ.Link()) {
      return linksOwnIndexOf(name);
    } else // patch
    {
      return patchesOwnIndexOf(name);
    }
  }

  public int indexOfVariable(Agent agent, String name) {
    if (agent instanceof Observer) {
      return observerOwnsIndexOf(name);
    } else if (agent instanceof Turtle) {
      AgentSet breed = ((Turtle) agent).getBreed();
      if (breed != _turtles) {
        int result = breedsOwnIndexOf(breed, name);
        if (result != -1) {
          return result;
        }
      }
      return turtlesOwnIndexOf(name);
    } else if (agent instanceof Link) {
      AgentSet breed = ((Link) agent).getBreed();
      if (breed != _links) {
        int result = linkBreedsOwnIndexOf(breed, name);
        if (result != -1) {
          return result;
        }
      }
      return linksOwnIndexOf(name);
    } else // patch
    {
      return patchesOwnIndexOf(name);
    }
  }

  public String turtlesOwnNameAt(int index) {
    return _program.turtlesOwn().apply(index);
  }

  public int turtlesOwnIndexOf(String name) {
    return _program.turtlesOwn().indexOf(name);
  }

  public int linksOwnIndexOf(String name) {
    return _program.linksOwn().indexOf(name);
  }

  public String linksOwnNameAt(int index) {
    return _program.linksOwn().apply(index);
  }

  int oldTurtlesOwnIndexOf(String name) {
    return _oldProgram.turtlesOwn().indexOf(name);
  }

  int oldLinksOwnIndexOf(String name) {
    return _oldProgram.linksOwn().indexOf(name);
  }

  public String breedsOwnNameAt(org.nlogo.api.AgentSet breed, int index) {
    Breed b = _program.breeds().apply(breed.printName());
    return b.owns().apply(index - _program.turtlesOwn().size());
  }

  public int breedsOwnIndexOf(AgentSet breed, String name) {
    Integer result = breedsOwnCache.get(breed.printName() + "~" + name);
    if (result == null)
      return -1;
    else
      return result.intValue();
  }

  public String linkBreedsOwnNameAt(AgentSet breed, int index) {
    Breed b = _program.linkBreeds().apply(breed.printName());
    return b.owns().apply(index - _program.linksOwn().size());
  }

  public int linkBreedsOwnIndexOf(AgentSet breed, String name) {
    Integer result = breedsOwnCache.get(breed.printName() + "~" + name);
    if (result == null)
      return -1;
    else
      return result.intValue();
  }

  /**
   * used by Turtle.realloc()
   */
  int oldBreedsOwnIndexOf(AgentSet breed, String name) {
    scala.Option[Breed] b = _oldProgram.breeds().get(breed.printName());
    if (b.isEmpty()) {
      return -1;
    }
    int result = b.get().owns().indexOf(name);
    if (result == -1) {
      return -1;
    }
    return _oldProgram.turtlesOwn().size() + result;
  }

  /**
   * used by Link.realloc()
   */
  int oldLinkBreedsOwnIndexOf(AgentSet breed, String name) {
    scala.Option[Breed] b = _oldProgram.linkBreeds().get(breed.printName());
    if (b.isEmpty()) {
      return -1;
    }
    int result = b.get().owns().indexOf(name);
    if (result == -1) {
      return -1;
    }
    return _oldProgram.linksOwn().size() + result;
  }

  public String patchesOwnNameAt(int index) {
    return _program.patchesOwn().apply(index);
  }

  public int patchesOwnIndexOf(String name) {
    return _program.patchesOwn().indexOf(name);
  }

  public String observerOwnsNameAt(int index) {
    return _program.globals().apply(index);
  }

  public int observerOwnsIndexOf(String name) {
    return _observer.variableIndex(name.toUpperCase());
  }

  /// breeds & shapes

  public boolean isBreed(AgentSet breed) {
    return _program.breeds().isDefinedAt(breed.printName());
  }

  public boolean isLinkBreed(AgentSet breed) {
    return _program.linkBreeds().isDefinedAt(breed.printName());
  }

  public AgentSet getBreed(String breedName) {
    return breeds.get(breedName);
  }

  public AgentSet getLinkBreed(String breedName) {
    return linkBreeds.get(breedName);
  }

  public String getBreedSingular(AgentSet breed) {
    if (breed == _turtles) {
      return "TURTLE";
    }

    String breedName = breed.printName();
    scala.Option[Breed] entry = _program.breeds().get(breedName);
    if (entry.nonEmpty()) {
      return entry.get().singular();
    } else {
      return "TURTLE";
    }
  }

  public String getLinkBreedSingular(AgentSet breed) {
    if (breed == _links) {
      return "LINK";
    }

    String breedName = breed.printName();
    scala.Option[Breed] entry = _program.linkBreeds().get(breedName);
    if (entry.nonEmpty()) {
      return entry.get().singular();
    } else {
      return "LINK";
    }
  }

  // assumes caller has already checked to see if the breeds are equal
  public int compareLinkBreeds(AgentSet breed1, AgentSet breed2) {
    for (JIterator[AgentSet] iter = linkBreeds.values().iterator();
         iter.hasNext();) {
      AgentSet next = iter.next();
      if (next == breed1) {
        return -1;
      } else if (next == breed2) {
        return 1;
      }
    }

    throw new IllegalStateException("neither of the breeds exist, that's bad");
  }

  public int getVariablesArraySize(Observer observer) {
    return _program.globals().size();
  }

  public int getVariablesArraySize(Patch patch) {
    return _program.patchesOwn().size();
  }

  public int getVariablesArraySize(org.nlogo.api.Turtle turtle, org.nlogo.api.AgentSet breed) {
    if (breed == _turtles) {
      return _program.turtlesOwn().size();
    } else {
      String breedName = breed.printName();
      Breed b = _program.breeds().apply(breedName);
      scala.collection.Seq[String] breedOwns = b.owns();
      return _program.turtlesOwn().size() + breedOwns.size();
    }
  }

  public int getVariablesArraySize(org.nlogo.api.Link link, org.nlogo.api.AgentSet breed) {
    if (breed == _links) {
      return _program.linksOwn().size();
    } else {
      scala.collection.Seq[String] breedOwns =
          _program.linkBreeds().apply(breed.printName()).owns();
      return _program.linksOwn().size() + breedOwns.size();
    }
  }

  public int getLinkVariablesArraySize(AgentSet breed) {
    if (breed == _links) {
      return _program.linksOwn().size();
    } else {
      scala.collection.Seq[String] breedOwns =
          _program.linkBreeds().apply(breed.printName()).owns();
      return _program.linksOwn().size() + breedOwns.size();
    }
  }

  public String checkTurtleShapeName(String name) {
    name = name.toLowerCase();
    if (turtleShapeList().exists(name)) {
      return name;
    } else {
      return null; // indicates failure
    }
  }

  public String checkLinkShapeName(String name) {
    name = name.toLowerCase();
    if (linkShapeList().exists(name)) {
      return name;
    } else {
      return null; // indicates failure
    }
  }

  public Shape getLinkShape(String name) {
    return linkShapeList().shape(name);
  }

  JMap[String, AgentSet] getAgentBreeds() {
    return breeds;
  }

  public JMap[String, ? extends org.nlogo.agent.AgentSet] getBreeds() {
    return breeds;
  }

  public boolean breedOwns(AgentSet breed, String name) {
    if (breed == _turtles) {
      return false;
    }
    return breedsOwnIndexOf(breed, name) != -1;
  }

  // TODO: Get rid of this method
  JMap[String, AgentSet] getLinkAgentBreeds() {
    return linkBreeds;
  }

  public JMap[String, ? extends org.nlogo.agent.AgentSet] getLinkBreeds() {
    return linkBreeds;
  }

  public boolean linkBreedOwns(AgentSet breed, String name) {
    if (breed == _links) {
      return false;
    }
    scala.collection.Seq[String] breedOwns =
        _program.linkBreeds().apply(breed.printName()).owns();
    return breedOwns.contains(name);
  }


  /// program

  private Program _program = newProgram();

  private Program _oldProgram = null;
  private JMap[String, AgentSet] oldBreeds     = new JHashMap[String, AgentSet]();
  private JMap[String, AgentSet] oldLinkBreeds = new JHashMap[String, AgentSet]();

  public Program program() {
    return _program;
  }

  Program oldProgram() {
    return _oldProgram;
  }

  public void program(Program program) {
    if (program == null) {
      throw new IllegalArgumentException
          ("World.program cannot be set to null");
    }
    _program = program;

    breeds.clear();
    linkBreeds.clear();
    createBreeds(_program.breeds(), breeds);
    createBreeds(_program.linkBreeds(), linkBreeds);
  }

  public Program newProgram() {
    Dialect dialect = org.nlogo.api.NetLogoLegacyDialect$.MODULE$;
    if (Version$.MODULE$.is3D()) {
      dialect = org.nlogo.api.NetLogoThreeDDialect$.MODULE$;
    }
    return org.nlogo.core.Program$.MODULE$.fromDialect(dialect);
  }

  public Program newProgram(scala.collection.Seq[String] interfaceGlobals) {
    Program emptyProgram = newProgram();
    return emptyProgram.copy(
        interfaceGlobals,
        emptyProgram.userGlobals(),
        emptyProgram.turtleVars(),
        emptyProgram.patchVars(),
        emptyProgram.linkVars(),
        emptyProgram.breeds(),
        emptyProgram.linkBreeds(),
        emptyProgram.dialect());
  }

  public void rememberOldProgram() {
    _oldProgram = _program;
    oldBreeds = new JHashMap[String, AgentSet](breeds);
    oldLinkBreeds = new JHashMap[String, AgentSet](linkBreeds);
  }

  /// display on/off

  private boolean displayOn = true;

  public boolean displayOn() {
    return displayOn;
  }

  public void displayOn(boolean displayOn) {
    this.displayOn = displayOn;
  }

  /// accessing observer variables by name;

  public Object getObserverVariableByName(final String var) {
    int index = _observer.variableIndex(var.toUpperCase());
    if (index >= 0) {
      return _observer.variables[index];
    }
    throw new IllegalArgumentException
        ("\"" + var + "\" not found");
  }

  public void setObserverVariableByName(final String var, Object value)
      throws AgentException, LogoException {
    int index = _observer.variableIndex(var.toUpperCase());
    if (index != -1) {
      _observer.setObserverVariable(index, value);
      return;
    }
    throw new IllegalArgumentException("\"" + var + "\" not found");
  }

  private CompilerServices _compiler;

  public void compiler_$eq(CompilerServices compiler) {
    _compiler = compiler;
  }

  public CompilerServices compiler() {
    return _compiler;
  }

  public scala.collection.Iterator[Object] allStoredValues() {
    return AllStoredValues.apply(this);
  }

  public static interface VariableWatcher {
    /**
     * Called when the watched variable is set.
     * @param agent The agent for which the variable was set
     * @param variableName The name of the variable as an upper case string
     * @param value The new value of the variable
     */
    public void update(Agent agent, String variableName, Object value);
  }

  // Variable watching *must* be done on variable name, not number. Numbers
  // can change in the middle of runs if, for instance, the user rearranges
  // the order of declarations in turtles-own and then keeps running.
  //
  // I didn't use SimpleChangeEvent here since I wanted the observers to know
  // what the change actually was.
  // -- BCH (4/1/2014)

  private JMap[String, JList[VariableWatcher]] variableWatchers = null;

  /**
   * A watcher to be notified every time the given variable changes for any agent.
   * @param variableName The variable name to watch as an upper case string; e.g. "XCOR"
   * @param watcher The watcher to notify when the variable changes
   */
  public void addWatcher(String variableName, VariableWatcher watcher) {
    if (variableWatchers == null) {
      variableWatchers =  new JHashMap[String, JList[VariableWatcher]]();
    }
    if (!variableWatchers.containsKey(variableName)) {
      variableWatchers.put(variableName, new CopyOnWriteArrayList[VariableWatcher]());
    }
    variableWatchers.get(variableName).add(watcher);
  }

  /**
   * Deletes a variable watcher.
   * @param variableName The watched variable name as an upper case string; e.g. "XCOR"
   * @param watcher The watcher to delete
   */
  public void deleteWatcher(String variableName, VariableWatcher watcher) {
    if (variableWatchers != null && variableWatchers.containsKey(variableName)) {
      JList[VariableWatcher] watchers = variableWatchers.get(variableName);
      watchers.remove(watcher);
      if (watchers.isEmpty()) {
        variableWatchers.remove(variableName);
      }
      if (variableWatchers.isEmpty()) {
        variableWatchers = null;
      }
    }
  }

  void notifyWatchers(Agent agent, int vn, Object value) {
    // This needs to be crazy fast if there are no watchers. Thus, null check. -- BCH (3/31/2014)
    if (variableWatchers != null) {
      String variableName = agent.variableName(vn);
      JList[VariableWatcher] watchers = variableWatchers.get(variableName);
      if (watchers != null) {
        for (VariableWatcher watcher : watchers) {
          watcher.update(agent, variableName, value);
        }
      }
    }
  }

  public void setUpShapes(boolean clearOld) {
    turtleBreedShapes.setUpBreedShapes(clearOld, breeds);
    linkBreedShapes.setUpBreedShapes(clearOld, linkBreeds);
  }

}
