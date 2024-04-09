package simpret


import simpret.lexer._
import simpret.parser._
import simpret.errors._
import simpret.interpreter._
import org.scalatest.funsuite.AnyFunSuite
import java.io.File


class MainTestInterpreter extends AnyFunSuite {

  def test_eval(filename: String, expected_result: Either[InterpreterError, AST]) = {
    assert(runcase(filename) === expected_result)
  }

  def getListOfFiles(dir: String, extensions: List[String]): List[File] = {
    new File(dir).listFiles.filter(_.isFile).toList.filter { file =>
      extensions.exists(file.getName.endsWith(_))
    }
  }

  def loadAst(filename: String): Either[InterpreterError, AST] = {
    for {
      input <- FileLoader(filename)
      tokens <- Lexer(input)
      ast <- Parser(tokens)
    } yield (ast)
  }

  def singleAutoTest(in: File) = {
    var inFilename = in.getAbsolutePath()
    var stepFilename = inFilename + ".step"
    var ioError = true
    for {
      inAst <- loadAst(inFilename)
      stepAst <- loadAst(stepFilename)
    } yield {
      assert(Interpreter.step(inAst) == Some(stepAst))
      ioError = false
    }
    assert(!ioError)
  }

  def fixAutoTest(in:File) = {
    var inFilename = in.getAbsolutePath()
    var fixFilename = inFilename + ".fix"
    var ioError = true
    for {
      inAst <- loadAst(inFilename)
      fixAst <- loadAst(fixFilename)
    } yield {
      ioError = false
      assert(Interpreter.eval(inAst) == fixAst)
    }
    assert(!ioError)
  }

  def runstep(filename: String): Either[InterpreterError, Option[AST]] = {
    for {
      ast <- loadAst(filename)
    } yield Interpreter.step(ast)
  }

  def runcase(filename: String): Either[InterpreterError, AST] = {
    for {
      ast <- loadAst(filename)
    } yield (Interpreter.eval(ast))
  }

}
