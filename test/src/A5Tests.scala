import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite
import cs241e.assignments.A5.printProcedure
import cs241e.assignments.A4.{const, getReg, printReg, getVar}
import cs241e.assignments.A5.printArray
import cs241e.assignments.A5.treeHeight
import cs241e.assignments.Reg.result

class A5TestsL2 extends AnyFunSuite {
  test("printprocedure") {
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = block(
      call(printProcedure, const(12)),
      call(printProcedure, const(-12)),
      call(printProcedure, const(-100000)),
      call(printProcedure, const(0)),
      call(printProcedure, const(Int.MinValue))
    )
    val machineCode = compilerA6(Seq(main, printProcedure))
    // val endState = A4.loadAndRun(
    //   machineCode,
    //   register1 = Word(encodeSigned(1)),
    //   register2 = Word(encodeSigned(2))
    // )
  }
  test("printArray") {
    val arr = Seq(
      Word(encodeSigned(12)),
      Word(encodeSigned(-12)),
      Word(encodeSigned(-100000)),
      Word(encodeSigned(0)),
      Word(encodeSigned(Int.MinValue))
    )
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = block(
      call(printArray, getVar(a), getVar(b))
    )
    val machineCode = compilerA6(Seq(main, printArray, printProcedure))
    val endState = A4.loadAndRunArray(machineCode, arr)
  }
  test("Tree") {
    val array = Seq(
      Word(encodeSigned(77)),
      Word(encodeSigned(3)),
      Word(encodeSigned(6)),
      Word(encodeSigned(22)),
      Word(encodeSigned(-1)),
      Word(encodeSigned(-1)),
      Word(encodeSigned(-8)),
      Word(encodeSigned(9)),
      Word(encodeSigned(12)),
      Word(encodeSigned(-36)),
      Word(encodeSigned(-1)),
      Word(encodeSigned(-1)),
      Word(encodeSigned(999)),
      Word(encodeSigned(-1)),
      Word(encodeSigned(-1))
    )
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = block(
      call(treeHeight(0), getVar(a), getVar(b))
    )
    val machineCode = compilerA6(main +: treeHeight :+ printProcedure)
    val endState = A4.loadAndRunArray(machineCode, array = array)
    println(endState.reg(result.number))
  }
}
