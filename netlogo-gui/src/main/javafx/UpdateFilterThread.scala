// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo(UTF8)

package org.nlogo.javafx

import java.util.concurrent.{ BlockingQueue, LinkedBlockingQueue }
import java.util.concurrent.atomic.AtomicReference

import org.nlogo.internalapi.{ ModelUpdate, WorldUpdate }
import org.nlogo.agent.World

// This thread handles filtering updates to keep the UI thread from getting behind on world updates
class UpdateFilterThread(worldUpdates: BlockingQueue[ModelUpdate]) extends Thread("Update Filter") {

  val filteredUpdates = new LinkedBlockingQueue[ModelUpdate]()
  var latestWorld = new AtomicReference[(World, Long)]((null, 0))

  def die(): Unit = {
    interrupt()
    join()
  }

  override def run(): Unit = {
    while (! Thread.interrupted) {
      worldUpdates.take() match {
        case WorldUpdate(world: World, t) => latestWorld.set((world, t))
        case other                        => filteredUpdates.put(other)
      }
    }
  }

}
