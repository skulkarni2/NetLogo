// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.job

import java.util.{ Comparator, UUID }
import java.util.concurrent.BlockingQueue

import org.nlogo.internalapi.{ AddProcedureRun, JobScheduler => ApiJobScheduler,
  ModelAction, StopProcedure, SuspendableJob, UpdateInterfaceGlobal }

object ScheduledJobThread {
  sealed trait ScheduledEvent {
    def submissionTime: Long
  }
  case class ScheduleOperation(op: () => Unit, tag: String, submissionTime: Long) extends ScheduledEvent
  case class StopJob(cancelTag: String, submissionTime: Long) extends ScheduledEvent
  case class AddJob(job: SuspendableJob, tag: String, submissionTime: Long) extends ScheduledEvent
  // Q: Why not have AddJob and RunJob be the same?
  // A: I don't think that's a bad idea, per se, but I want to allow flexibility in the future.
  //    Having RunJob events created only the RunJob event to hold additional information
  //    about the job including things like "how long since it was run last" and "how long
  //    is it estimated to run for" that isn't available when adding the job. RG 3/28/17
  case class RunJob(job: SuspendableJob, tag: String, submissionTime: Long) extends ScheduledEvent

  object PriorityOrder extends Comparator[ScheduledEvent] {
    // lower means "first" or higher priority
    def basePriority(e: ScheduledEvent): Int = {
      e match {
        case ScheduleOperation(_, _, _) => 0
        case StopJob(_, _)              => 1
        case AddJob(_, _, _)            => 2
        case RunJob(_, _, _)            => 3
      }
    }
    def compare(e1: ScheduledEvent, e2: ScheduledEvent): Int = {
      val p1 = basePriority(e1)
      val p2 = basePriority(e2)
      if (p1 < p2) -1
      else if (p1 > p2) 1
      else if (e1.submissionTime < e2.submissionTime) -1
      else if (e1.submissionTime > e2.submissionTime) 1
      else 0
    }
  }
}

import ScheduledJobThread._

trait JobScheduler extends ApiJobScheduler {
  def queue: BlockingQueue[ScheduledEvent]

  def scheduleJob(job: SuspendableJob): String = {
    val jobTag = UUID.randomUUID.toString
    val e = AddJob(job, jobTag, System.currentTimeMillis)
    queue.add(e)
    jobTag
  }

  def scheduleOperation(op: () => Unit): String = {
    val jobTag = UUID.randomUUID.toString
    val e = ScheduleOperation(op, jobTag, System.currentTimeMillis)
    queue.add(e)
    jobTag
  }

  def stopJob(jobTag: String): Unit = {
    queue.add(StopJob(jobTag, System.currentTimeMillis))
  }

  // this should maybe take a timeout parameter?
  def runEvent(): Unit = {
    queue.poll() match {
      case AddJob(j, t, time) => queue.add(RunJob(j, t, System.currentTimeMillis))
      case _ =>
    }
  }
}
