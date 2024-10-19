import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.{Chunk, Stack}
import cs241e.assignments.ProgramRepresentation.{write, *}
import cs241e.assignments.Transformations.*
import cs241e.assignments.{A1, A4, Assembler, Debugger, Reg}
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite
import cs241e.assignments.A4.arrayMaximum
import cs241e.assignments.A4.outputLetters
import cs241e.assignments.A4.printInteger
import cs241e.assignments.Debugger.debug

class A4TestsT1 extends AnyFunSuite {
  // test("scala") {
  //   def proc() = {
  //     var v = 5
  //     if (1 + 1 == 2) v = 2 * (3 + v) else v = 42
  //     v
  //   }
  //   println(proc())
  // }
  // def const(i: Int) = block(
  //   LIS(Reg.result),
  //   Word(encodeSigned(i))
  // )
  // test("lacs") {
  //   val v = new Variable("v")
  //   val code = Scope(
  //     Seq(v),
  //     block(
  //       assign(v, const(5)), // v = 5
  //       Comment("now running if statememt"),
  //       ifStmt(
  //         binOp(const(2), plus, const(1)),
  //         eqCmp,
  //         const(2),
  //         assign(
  //           v,
  //           binOp(const(2), times, binOp(const(3), plus, read(Reg.result, v)))
  //         ),
  //         assign(v, const(42))
  //       ),
  //       read(Reg.result, v)
  //     )
  //   )
  //   //    disassemblingCodePrinter.pprintln(code)
  //   val machineCode = compilerA4(code)
  //   val finalState = A4.loadAndRun(machineCode, debug = true)
  //   println(decodeSigned(finalState.reg(3)))
  // }
  // test("arrayScala") {
  //   val array = Array(2, 4, 6, 8)
  //   println(array(2))
  // }
  // test("arrayLacs") {
  //   val array = Seq[Word](
  //     Word(encodeSigned(2)),
  //     Word(encodeSigned(4)),
  //     Word(encodeSigned(6)),
  //     Word(encodeSigned(8))
  //   )
  //   val arrayAddr = new Variable("arrayAddr")
  //   val size = new Variable("size")
  //   val code = Scope(
  //     Seq(arrayAddr, size),
  //     block(
  //       write(arrayAddr, Reg(1)),
  //       write(size, Reg(2)),
  //       deref(
  //         binOp(
  //           read(Reg.result, arrayAddr),
  //           plus,
  //           binOp(const(4), times, const(2))
  //         )
  //       )
  //     )
  //   )
  //   val machineCode = compilerA4(code)
  //   val finalState = A4.loadAndRunArray(machineCode, array)
  //   println(decodeSigned(finalState.reg(3)))
  // }
  test("max arr") {
    val array = Seq[Word](
      Word(encodeSigned(2)),
      Word(encodeSigned(4)),
      Word(encodeSigned(9)),
      Word(encodeSigned(8))
    )
    val finalState = A4.loadAndRunArray(arrayMaximum, array, debug = true)
    println(decodeSigned(finalState.reg(3)))
  }
  // test("output letters") {
  //   val array = Seq[Word](
  //     Word(encodeSigned(2)),
  //     Word(encodeSigned(4)),
  //     Word(encodeSigned(0)),
  //     Word(encodeSigned(8))
  //   )
  //   val finalState = A4.loadAndRunArray(outputLetters, array)
  // }
  test("print int") {
    val finalState0 = A4.loadAndRun(
      printInteger,
      register1 = Word(encodeSigned(0))
    )
    val finalState1 = A4.loadAndRun(
      printInteger,
      register1 = Word(encodeSigned(12))
    )
    val finalState2 = A4.loadAndRun(
      printInteger,
      register1 = Word(encodeSigned(-12))
    )
    val finalState3 = A4.loadAndRun(
      printInteger,
      register1 = Word(encodeSigned(-10000))
    )
    val finalState5 = A4.loadAndRun(
      printInteger,
      register1 = Word(encodeSigned(-2147483648))
    )
  }
}
