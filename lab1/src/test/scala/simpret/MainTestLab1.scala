package simpret

import simpret.errors._
import simpret.parser._


class MainTestLab1 extends MainTestInterpreter {
  val filesStep = getListOfFiles("src/test/sint/lab1/steptests", List("sint")).sorted
  val filesFix = getListOfFiles("src/test/sint/lab1/fixpointtests", List("sint")).sorted

  filesStep.foreach { file =>
    test("step test " + file.getName()) {
      singleAutoTest(file)
    }
  }

  filesFix.foreach { file =>
    test("fixpoint test " + file.getName()) {
      fixAutoTest(file)
    }
  }


  test("TestCaseLab1 0 COND") {
    test_eval("src/test/sint/lab1/case_0_cond_0.sint", Right(IntLit(11)))
    test_eval("src/test/sint/lab1/case_0_cond_1.sint", Right(IntLit(12)))
    // here evaluation gets stuck
    test_eval("src/test/sint/lab1/case_0_cond_2.sint", Right(CondExp(IntLit(-1),IntLit(11),IntLit(12))))
  }


  // this is how you add a test case
  test("TestCaseLab1 1 ABC") {
    // here you add the sub testcases
  }

  // this is how you add another test case
  test("TestCaseLab1 2 XYZ") {
    // here you add the corresponding sub testcases
  }

  test("bonus test") {

  val program = "(\\y. y) (\\y. y) z"

  val sol = "(\\y. y) z"

  val programAST = simpret.lexer.Lexer(program).flatMap(simpret.parser.Parser(_)).toOption.get

  val solAST = simpret.lexer.Lexer(sol).flatMap(simpret.parser.Parser(_)).toOption.get

  assert(simpret.interpreter.Interpreter.eval(programAST) == solAST)

}
}
