// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.compile.api

import org.nlogo.core.{ Command => CoreCommand, Reporter => CoreReporter,
  SourceLocation, Syntax }
import org.nlogo.core.prim.{ _const => _coreconst }
import org.nlogo.nvm.{ Command, Context, Instruction, Reporter }
import org.scalatest.{ FunSuite, Inside }

class PrimitiveMungersTests extends FunSuite with Inside {
  class DummyCoreCommand(rights: Int*) extends CoreCommand {
    def syntax = Syntax.commandSyntax(right = rights.toList)
  }
  class DummyCoreReporter extends CoreReporter {
    def syntax = Syntax.reporterSyntax(ret = Syntax.NumberType)
  }
  class DummyCommand extends Command {
    def perform(c: Context): Unit = {}
  }
  class DummyReporter extends Reporter {
    def report(c: Context): AnyRef = Double.box(0.0)
  }

  def newMatch(a: AstNode): Match = {
    new Match(a)
  }

  val constNum = {
    new ReporterApp(new _coreconst(Double.box(0.0)), new DummyReporter(), Seq(), new SourceLocation(0, 0, ""))
  }

  def statement(cc: CoreCommand, c: Command, args: Expression*): Statement = {
    new Statement(cc, c, args, new SourceLocation(0, 0, ""))
  }

  def repApp(cr: CoreReporter, r: Reporter): ReporterApp = {
    new ReporterApp(cr, r, Seq(), new SourceLocation(0, 0, ""))
  }

  test("match.command returns the statement command") {
    val command = new DummyCommand()
    val m = newMatch(statement(new DummyCoreCommand(), command))
    assert(m.command == command)
  }

  test("match.replace replaces the root instruction") {
    val command = new DummyCommand()
    val m = newMatch(statement(new DummyCoreCommand(), command))
    val command2 = new DummyCommand()
    m.replace(command2)
    assert(m.command eq command2)
  }

  test("match.strip removes each argument to the node") {
    val command = new DummyCommand()
    val m = newMatch(statement(new DummyCoreCommand(), command, constNum))
    m.strip()
    inside(m.node) { case s: Statement => assert(s.args.length == 0) }
  }

  test("matchEmptyCommandBlockIsLastArg matches empty blocks") {
    val command = new DummyCommand()
    val emptyBlock = new StatementsBuilder { }
    val m = newMatch(statement(new DummyCoreCommand(Syntax.CommandBlockType), command, emptyBlock.buildBlock))
    m.matchEmptyCommandBlockIsLastArg
  }

  test("matchEmptyCommandBlockIsLastArg fails on non-empty block") {
    val command = new DummyCommand()
    val emptyBlock = new StatementsBuilder {
      statement(new DummyCoreCommand(), new DummyCommand())
    }
    val m = newMatch(statement(new DummyCoreCommand(Syntax.CommandBlockType), command, emptyBlock.buildBlock))
    intercept[MatchFailedException] {
      m.matchEmptyCommandBlockIsLastArg
    }
  }

  test("matchArg fails when there are no arguments") {
    pending
  }

  test("matchArg succeeds when there is an argument at its index") {
    pending
  }

  test("matchArg fails when there is no argument at its index") {
    pending
  }

  test("match.reporter returns the ReporterApp reporter") {
    val reporter = new DummyReporter()
    val m = newMatch(repApp(new DummyCoreReporter(), reporter))
    assert(m.reporter == reporter)
  }
}
