// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo(UTF8)

package org.nlogo.javafx

import javafx.scene.canvas.GraphicsContext

import org.nlogo.api.{ GraphicsInterface => ApiGraphicsInterface }

class GraphicsInterface(context: GraphicsContext) extends ApiGraphicsInterface {
  def antiAliasing(on: Boolean): Unit = { }
  def dispose(): Unit = { }
  def draw(shape: java.awt.Shape): Unit = ???
  def drawCircle(x: Double,y: Double,xDiamter: Double,yDiameter: Double,scale: Double,angle: Double): Unit = {
    // need scale, angle...
    context.strokeOval(x, y, xDiamter, yDiameter)
  }
  def drawImage(image: java.awt.Image,x: Int,y: Int,width: Int,height: Int): Unit = ???
  def drawImage(image: java.awt.image.BufferedImage): Unit = ???
  def drawLabel(s: String,x: Double,y: Double,patchSize: Double): Unit = ???
  def drawLine(x1: Double,y1: Double,x2: Double,y2: Double): Unit = ???
  def drawPolygon(xcors: Array[Int],ycors: Array[Int],length: Int): Unit = ???
  def drawPolyline(xcors: Array[Int],ycors: Array[Int],length: Int): Unit = ???
  def drawRect(x: Double,y: Double,width: Double,height: Double,scale: Double,angle: Double): Unit = ???
  def fill(shape: java.awt.Shape): Unit = ???
  def fillCircle(x: Double,y: Double,xDiameter: Double,yDiameter: Double,scale: Double,angle: Double): Unit = ???
  def fillPolygon(xcors: Array[Int],ycors: Array[Int],length: Int): Unit = ???
  def fillRect(x: Double,y: Double,width: Double,height: Double,scale: Double,angle: Double): Unit = ???
  def fillRect(x: Int,y: Int,width: Int,height: Int): Unit = ???
  def getFontMetrics: java.awt.FontMetrics = ???
  def location(x: Double,y: Double): String = ???
  def pop(): Unit = ???
  def push(): Unit = ???
  def rotate(theta: Double,x: Double,y: Double,offset: Double): Unit = ???
  def rotate(theta: Double,x: Double,y: Double): Unit = ???
  def rotate(theta: Double): Unit = ???
  def scale(x: Double,y: Double,shapeWidth: Double): Unit = ???
  def scale(x: Double,y: Double): Unit = ???
  def setColor(c: java.awt.Color): Unit = ???
  def setComposite(comp: java.awt.Composite): Unit = ???
  def setInterpolation(): Unit = ???
  def setStroke(width: Float,dashes: Array[Float]): Unit = ???
  def setStroke(width: Double): Unit = ???
  def setStrokeControl(): Unit = ???
  def setStrokeFromLineThickness(lineThickness: Double,scale: Double,cellSize: Double,shapeWidth: Double): Unit = ???
  def translate(x: Double,y: Double): Unit = ???
}
