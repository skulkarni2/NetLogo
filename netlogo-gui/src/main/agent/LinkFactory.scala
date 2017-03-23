// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

abstract class LinkFactory[W <: CoreWorld] {
  def apply(world: W, src: Turtle, dest: Turtle, breed: AgentSet): Link
}
