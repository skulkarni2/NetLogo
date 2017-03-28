// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.job

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.LinkedBlockingQueue
import java.util.{ Collections, ArrayList }

import org.nlogo.internalapi.{ ModelAction, UpdateInterfaceGlobal, AddProcedureRun, StopProcedure, SuspendableJob }

import org.scalatest.{ FunSuite, Inside }

import scala.collection.JavaConverters._

import ScheduledJobThread._

class ScheduledJobThreadTest extends FunSuite {

  def sortEvents(es: ScheduledEvent*): Seq[ScheduledEvent] = {
    val a = new ArrayList[ScheduledEvent](es.length)
    es.foreach(a.add)
    Collections.sort(a, ScheduledJobThread.PriorityOrder)
    a.asScala
  }

  def assertSortedOrder(e1: ScheduledEvent, e2: ScheduledEvent): Unit = {
    assert(sortEvents(e1, e2) == Seq(e1, e2))
    assert(sortEvents(e2, e1) == Seq(e1, e2))
  }

  test("job ordering puts scheduled operation ahead of stopping a job") {
    assertSortedOrder(ScheduleOperation({() => }, "abc", 1), StopJob("a", 0))
  }

  test("job ordering puts scheduled operation job ahead of adding a job") {
    assertSortedOrder(StopJob("a", 1), AddJob(null, "abc", 0))
  }

  test("job ordering puts the oldest job stop first"){
    assertSortedOrder(StopJob("a", 1), StopJob("a", 2))
  }

  test("job ordering puts the oldest add job first"){
    assertSortedOrder(AddJob(null, "abc", 0), AddJob(null, "abc", 1))
  }

  test("job ordering puts run job behind adding a job"){
    assertSortedOrder(AddJob(null, "abc", 0), RunJob(null, "abc", 1))
  }

  // NOTE: if the job thread is ever expanded to include secondary or intermittent
  // jobs, this ordering should be tweaked
  test("job ordering puts the oldest run job first"){
    assertSortedOrder(RunJob(null, "abc", 0), RunJob(null, "abc", 1))
  }

  class Subject extends JobScheduler {
    val queue = new LinkedBlockingQueue[ScheduledEvent]
  }
  object DummyOneRunJob extends SuspendableJob {
    def runFor(steps: Int): Option[SuspendableJob] = None
  }

  trait Helper extends Inside {
    val subject = new Subject()
    def firstEvent = subject.queue.peek()
  }

  test("adding a job schedules the job to be run") { new Helper {
    val jobTag = subject.scheduleJob(DummyOneRunJob)
    inside(firstEvent) { case AddJob(_, t, time) => assert(t == jobTag) }
  } }

  test("scheduling an operation schedules it for run") { new Helper {
    val jobTag = subject.scheduleOperation({ () => })
    inside(firstEvent) { case ScheduleOperation(_, t, time) => assert(t == jobTag) }
  } }

  test("stopping a job schedules a job stop") { new Helper {
    subject.stopJob("abc")
    inside(firstEvent) { case StopJob(cancelTag, _) => assert(cancelTag == "abc") }
  } }

  test("running a job addition causes a scheduled job to be added") { new Helper {
    val jobTag = subject.scheduleJob(DummyOneRunJob)
    subject.runEvent()
    inside(firstEvent) { case RunJob(j, t, time) =>
      assert(j == DummyOneRunJob)
      assert(t == jobTag)
    }
  } }

  test("a job stop prevents a scheduled job from being run") { new Helper {
    pending
  } }

  test("if a job stop is processed before the job is added, the job is never added"){
    pending
  }

  test("schedules the continuation of a job if the job returns a continuation"){
    pending
  }
}
