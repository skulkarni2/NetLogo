// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo(UTF8)

package org.nlogo.javafx

import javafx.scene.canvas.GraphicsContext

import org.nlogo.api.{ GraphicsInterface => ApiGraphicsInterface }

class GraphicsInterface(context: GraphicsContext) extends ApiGraphicsInterface {
  // TODO: I believe scale is correcting for the size ratio of Shape.Width. I'm not sure context.scale accounts for this properly...
  def antiAliasing(on: Boolean): Unit = { }
  def dispose(): Unit = { }

  def draw(shape: java.awt.Shape): Unit = ???

  def drawCircle(x: Double,y: Double,xDiamter: Double,yDiameter: Double,scale: Double,angle: Double): Unit = {
    // need scale, angle...
    context.save()
    context.scale(scale, scale)
    context.rotate(angle)
    context.strokeOval(x, y, xDiamter, yDiameter)
    context.restore()
  }
  // javafx has similar calls, but we'll need to transform from an awt Image / BufferedImage to
  // a javafx image
  def drawImage(image: java.awt.Image,x: Int,y: Int,width: Int,height: Int): Unit = ???

  def drawImage(image: java.awt.image.BufferedImage): Unit = ???

  def drawLabel(s: String,x: Double,y: Double,patchSize: Double): Unit = {
    // TODO: this should be adjusted for max ascent / descent
    context.fillText(s, 0, 0)
  }
  def drawLine(x1: Double,y1: Double,x2: Double,y2: Double): Unit = {
    context.strokeLine(x1, y1, x2, y2)
  }
  def drawPolygon(xcors: Array[Int],ycors: Array[Int],length: Int): Unit = {
    context.strokePolygon(
      xcors.map(_.toDouble).toArray[Double],
      ycors.map(_.toDouble).toArray[Double],
      length)
  }
  def drawPolyline(xcors: Array[Int],ycors: Array[Int],length: Int): Unit = {
    context.strokePolyline(
      xcors.map(_.toDouble).toArray[Double],
      ycors.map(_.toDouble).toArray[Double],
      length)
  }
  def drawRect(x: Double,y: Double,width: Double,height: Double,scale: Double,angle: Double): Unit = {
    context.save()
    context.scale(scale, scale)
    context.rotate(angle)
    context.strokeRect(x, y, width, height)
    context.restore()
  }
  def fill(shape: java.awt.Shape): Unit = ???

  def fillCircle(x: Double,y: Double,xDiameter: Double,yDiameter: Double,scale: Double,angle: Double): Unit = {
    context.save()
    context.scale(scale, scale)
    context.rotate(angle)
    context.fillOval(x, y, xDiameter, yDiameter)
    context.restore()
  }
  def fillPolygon(xcors: Array[Int],ycors: Array[Int],length: Int): Unit = {
    context.fillPolygon(
      xcors.map(_.toDouble).toArray[Double],
      ycors.map(_.toDouble).toArray[Double],
      length)
  }
  def fillRect(x: Double,y: Double,width: Double,height: Double,scale: Double,angle: Double): Unit = {
    context.save()
    context.scale(scale, scale)
    context.rotate(angle)
    context.fillRect(x, y, width, height)
    context.restore()
  }
  def fillRect(x: Int,y: Int,width: Int,height: Int): Unit = {
    context.fillRect(x, y, width, height)
  }
  def getFontMetrics: java.awt.FontMetrics = ???
  def location(x: Double,y: Double): String = {
    s"(${context.getTransform.getTx + x} , ${context.getTransform.getTy + y})"
  }
  def pop(): Unit = {
    // TODO: This should also restore strokes saved by the push method
    context.restore()
  }
  def push(): Unit = {
    context.save()
    // TODO: This should also save strokes set with the setStroke methods
  }
  def rotate(theta: Double,x: Double,y: Double,offset: Double): Unit = {
    rotate(theta, x + offset / 2, y + offset / 2)
  }
  def rotate(theta: Double,x: Double,y: Double): Unit = {
    context.translate(x, y)
    context.rotate(theta)
    context.translate(-x, -y)
  }
  def rotate(theta: Double): Unit = {
    context.rotate(theta)
  }
  def scale(x: Double,y: Double,shapeWidth: Double): Unit = {
    context.scale(x / shapeWidth, y / shapeWidth)
  }
  def scale(x: Double,y: Double): Unit = {
    context.scale(x, y)
  }
  def setColor(c: java.awt.Color): Unit = {
    val jfxColor =
      javafx.scene.paint.Color.rgb(c.getRed, c.getGreen, c.getBlue)
    context.setStroke(jfxColor)
    context.setFill(jfxColor)
  }
  def setComposite(comp: java.awt.Composite): Unit = {
    // TODO: I'm not sure whether this is necessary and/or possible in JavaFX
  }
  def setInterpolation(): Unit = {
    // TODO: I'm not sure whether this is necessary and/or possible in JavaFX
  }
  def setStroke(width: Float,dashes: Array[Float]): Unit = {
    // also should set line cap, color
    context.setLineWidth(width)
    context.setLineDashes(dashes.map(_.toDouble): _*)
  }
  def setStroke(width: Double): Unit = {
    context.setLineWidth(width max 1.0)
    context.setLineDashes(1.0, 0.0)
  }
  def setStrokeControl(): Unit = {
    // TODO: Rendering hints about stroke
  }
  def setStrokeFromLineThickness(lineThickness: Double,scale: Double,cellSize: Double,shapeWidth: Double): Unit = ???
  def translate(x: Double,y: Double): Unit = {
    context.translate(x, y)
  }
}