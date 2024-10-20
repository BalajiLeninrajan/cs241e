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
import cs241e.assignments.Reg.stackPointer

class regular241 extends AnyFunSuite {
  test("A3Q4 recursion") {
    val funcLabel = new Label("recursive function")
    val allZeroBaseCase = new Label("Both 0")
    val oneZeroBaseCase = new Label("One 0")
    val endLabel = new Label("end")
    val myCode = block(
      Define(funcLabel),
      // prologue
      LIS(Reg.scratch),
      Word(encodeUnsigned(8)),
      Assembler.SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      Assembler.SW(Reg.link, 0, Reg.stackPointer),
      Assembler.SW(Reg.targetPC, 4, Reg.stackPointer),
      //
      // Check for base cases
      Assembler.ADD(Reg.scratch, Reg.input1, Reg.input2),
      beq(Reg.scratch, Reg.zero, allZeroBaseCase),
      beq(Reg.input1, Reg.zero, oneZeroBaseCase),
      beq(Reg.input2, Reg.zero, oneZeroBaseCase),
      //
      // function's main loop
      LIS(Reg.scratch),
      Word(encodeUnsigned(8)),
      Assembler.SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      Assembler.SW(Reg.input1, 0, Reg.stackPointer),
      Assembler.SW(Reg.input2, 4, Reg.stackPointer),
      // f(n - 1, k)
      LIS(Reg.scratch),
      Word(encodeSigned(-1)),
      Assembler.ADD(Reg.input1, Reg.input1, Reg.scratch),
      LIS(Reg.targetPC),
      Use(funcLabel),
      Assembler.JALR(Reg.targetPC),
      // (n - 1) * f(n - 1, k)
      Assembler.MULT(Reg.input1, Reg.result),
      Assembler.MFLO(Reg.result),
      LIS(Reg.scratch),
      Word(encodeUnsigned(4)),
      Assembler.SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      Assembler.SW(Reg.result, 0, Reg.stackPointer),
      // f(n - 1, k - 1)
      LIS(Reg.scratch),
      Word(encodeSigned(-1)),
      Assembler.ADD(Reg.input2, Reg.input2, Reg.scratch),
      Assembler.JALR(Reg.targetPC),
      // putting the 2 together
      Assembler.LW(Reg.scratch, 0, Reg.stackPointer),
      Assembler.ADD(Reg.result, Reg.result, Reg.scratch),
      LIS(Reg.scratch),
      Word(encodeUnsigned(4)),
      Assembler.ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      // restore inputs
      Assembler.LW(Reg.input1, 0, Reg.stackPointer),
      Assembler.LW(Reg.input2, 4, Reg.stackPointer),
      LIS(Reg.scratch),
      Word(encodeUnsigned(8)),
      Assembler.ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      beq(Reg.zero, Reg.zero, endLabel),
      //
      // base case 1
      Define(allZeroBaseCase),
      LIS(Reg.result),
      Word(encodeSigned(1)),
      beq(Reg.zero, Reg.zero, endLabel),
      // base case 2
      Define(oneZeroBaseCase),
      LIS(Reg.result),
      Word(encodeSigned(0)),
      // epilogue
      Define(endLabel),
      LIS(Reg.scratch),
      Word(encodeUnsigned(8)),
      Assembler.LW(Reg.link, 0, Reg.stackPointer),
      Assembler.LW(Reg.targetPC, 4, Reg.stackPointer),
      Assembler.ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      Assembler.JR(Reg.link)
    )

    val machineCode = compilerA4(myCode);
    println()
    val finalState =
      A4.loadAndRun(
        machineCode,
        Word(encodeSigned(5)),
        Word(encodeSigned(1))
      )
  }
  test("add overflow") {
    val code = block(
      Assembler.ADD(Reg.result, Reg.input1, Reg.input2)
    )
    val machineCode = compilerA4(code)
    val finalState =
      A4.loadAndRun(
        machineCode,
        Word("10000000000000000000000000000000"),
        Word("11111111111111111111111111111111")
      )
    println(finalState.reg(3))
  }
  test("check 0") {
    val code = Assembler.ADD(Reg.zero, Reg.input1, Reg.input2)
    val machineCode = compilerA4(code)
    println(
      A4.loadAndRun(machineCode, Word(encodeSigned(1)), Word(encodeSigned(2)))
    )
  }
}
