/*
   Copyright 2024 Ondrej Lhotak. All rights reserved.

   Permission is granted for private study use by students registered in
   CS 241E in the Fall 2024 term.

   The contents of this file may not be published, in whole or in part,
   in print or electronic form.

   The contents of this file may be included in work submitted for CS
   241E assignments in Fall 2024. The contents of this file may not be
   submitted, in whole or in part, for credit in any other course.
 */
package cs241e.assignments

import cs241e.mips.*
import Assembler.*
import Transformations.*
import ProgramRepresentation.*
import CodeBuilders.*
import A1.*
import cs241e.assignments.Reg.zero
import cs241e.assignments.Reg.scratch
import cs241e.assignments.Reg.result
import cs241e.assignments.CodeBuilders.binOp
import cs241e.assignments.Reg.stackPointer

object A4 {

  /** This is an enhanced version of the loadAndRun method from Assignment 1.
    *
    * It loads the give machine language code into memory, writes the specified
    * values to registers 1 and 2, and then runs the CPU on the code that was
    * loaded. The debugger can be invoked by passing the argument debug = true.
    */
  def loadAndRun(
      code: MachineCode,
      register1: Word = Word.zero,
      register2: Word = Word.zero,
      debug: Boolean = false
  ): State = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    if (debug) Debugger.debug(initialState, code.debugTable)
    else CPU.run(initialState)
  }

  /** A utility method that takes two sequences of words representing `code` and
    * an `array`, loads both into memory, and sets register 1 to the address of
    * the beginning of the array and register 2 to its length in words.
    */
  def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
    val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
    val arrayLength: Word = Word(encodeUnsigned(array.size))
    val loadedCode: State = setMem(code)
    val loadedArray: State = setMem(array, loadedCode, arrayAddress)
    loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
  }

  /** A utility method that loads code and an array into memory and runs the
    * code. The debugger can be invoked by passing the argument debug = true.
    */
  def loadAndRunArray(
      code: MachineCode,
      array: Seq[Word],
      debug: Boolean = false
  ): State = {
    val state = loadCodeAndArray(code.words, array)
    if (debug) Debugger.debug(state, code.debugTable)
    else CPU.run(state)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit
    * integers. Register 2 holds the number of elements in the array. If the
    * array is empty place the value -1 in register 3. Otherwise copy the last
    * element of the array into register 3.
    */
  lazy val lastElement: MachineCode = {
    val endLabel = new Label("end")
    val code: Code = block(
      LIS(Reg(3)),
      Word(encodeSigned(-1)),
      beq(Reg(2), zero, endLabel), // jump to end if list is empty
      ADD(scratch, Reg(3), Reg(2)), // store index in scratch
      LIS(Reg(3)),
      Word(encodeSigned(4)),
      MULT(scratch, Reg(3)), // multiply index by 4 to get offset
      MFLO(scratch),
      ADD(scratch, Reg(1), scratch),
      LW(Reg(3), 0, scratch),
      Define(endLabel)
    )
    compilerA4(code)
  }

  def const(i: Int) = block(
    LIS(Reg.result),
    Word(encodeSigned(i))
  )

  def getReg(r: Reg) = ADD(result, zero, r)

  def getVar(v: Variable) = read(result, v)

  def getArrElem(startAddr: Reg, i: Variable) = block(
    binOp(read(result, i), times, const(4)),
    ADD(result, result, startAddr),
    LW(result, 0, result)
  )

  def printReg(r: Reg) = block(
    LIS(scratch),
    CPU.printAddr,
    SW(r, 0, scratch)
  )

  /** Register 1 holds the address of the beginning of an array of 32-bit
    * two's-complement integers. Register 2 holds the number of elements in the
    * array. Determine the maximum of all the elements of the array, write it
    * into register 3. Assume the array is not empty.
    */
  lazy val arrayMaximum: MachineCode = {
    val index = new Variable("index")
    val arrMax = new Variable("max")
    val cur = new Variable("cur")

    val code: Code = Scope(
      Seq(index, arrMax, cur),
      block(
        assign(index, const(0)),
        assign(arrMax, getArrElem(Reg(1), index)),
        whileLoop(
          read(result, index),
          ltCmp,
          getReg(Reg(2)),
          block(
            assign(cur, getArrElem(Reg(1), index)),
            ifStmt(
              read(result, cur),
              gtCmp,
              read(result, arrMax),
              assign(arrMax, read(result, cur))
            ),
            assign(
              index,
              binOp(
                read(result, index),
                plus,
                const(1)
              )
            )
          )
        ),
        read(Reg(3), arrMax)
      )
    )
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit
    * integers, each representing a character. The integer zero represents a
    * space, and each integer i (1 <= i <= 26) represents the i'th letter of the
    * uppercase alphabet. Register 2 holds the number of elements in the array
    * (can be empty). Your program should output the uppercase characters
    * represented by the integers in the array. The MIPS system allows you to
    * output one character at a time, by storing its ASCII value into the
    * special memory location CPU.printAddr (1111 1111 1111 1111 0000 0000 0000
    * 1100).
    *
    * Hint: use Google to find an ASCII code table.
    */

  lazy val outputLetters: MachineCode = {
    val index = new Variable("index")
    val cur = new Variable("cur")

    val code: Code = Scope(
      Seq(index, cur),
      block(
        assign(index, const(0)),
        whileLoop(
          read(result, index),
          ltCmp,
          getReg(Reg(2)),
          block(
            assign(cur, getArrElem(Reg(1), index)),
            ifStmt(
              read(result, cur),
              eqCmp,
              const(0),
              block(
                const(32),
                printReg(result)
              ),
              block(
                binOp(
                  read(result, cur),
                  plus,
                  const(64)
                ),
                printReg(result)
              )
            ),
            assign(
              index,
              binOp(
                read(result, index),
                plus,
                const(1)
              )
            )
          )
        )
      )
    )
    compilerA4(code)
  }

  /** Register 1 holds a 32-bit integer (in two's-complement notation). Your
    * program should format this integer in base 10, print it, then print a
    * newline character.
    */

  lazy val printIntegerCode: Code = {
    val curVal = new Variable("current")
    val frameStartPtr = new Variable("fsp")
    Scope(
      Seq(curVal, frameStartPtr),
      block(
        assign(curVal, getReg(Reg(1))),
        assign(frameStartPtr, getReg(stackPointer)),
        ifStmt(
          read(result, curVal),
          eqCmp,
          const(-2147483648),
          block(
            const(45),
            printReg(result),
            assign(curVal, const(214748364))
          )
        ), // ifStmt
        ifStmt(
          read(result, curVal),
          eqCmp,
          const(0),
          block(
            const(48),
            printReg(result)
          )
        ),
        ifStmt(
          read(result, curVal),
          ltCmp,
          const(0),
          block(
            const(45),
            printReg(result),
            assign(
              curVal,
              binOp(
                read(result, curVal),
                times,
                const(-1)
              )
            )
          )
        ), // ifStmt
        whileLoop(
          read(result, curVal),
          gtCmp,
          const(0),
          block(
            const(4),
            SUB(stackPointer, stackPointer, result),
            binOp(
              read(result, curVal),
              remainderUnsigned,
              const(10)
            ),
            SW(result, 0, stackPointer),
            assign(
              curVal,
              binOp(
                read(result, curVal),
                divideUnsigned,
                const(10)
              )
            )
          )
        ), // whileLoop
        whileLoop(
          getReg(stackPointer),
          ltCmp,
          read(result, frameStartPtr),
          block(
            binOp(
              LW(result, 0, stackPointer),
              plus,
              const(48)
            ),
            printReg(result),
            const(4),
            ADD(stackPointer, stackPointer, result)
          )
        ), // whileLoop
        ifStmt(
          getReg(Reg(1)),
          eqCmp,
          const(-2147483648),
          block(
            const(56),
            printReg(result)
          )
        ), // ifStmt
        const(10),
        printReg(result)
      )
    )
  }

  lazy val printInteger: MachineCode = compilerA4(printIntegerCode)
}
