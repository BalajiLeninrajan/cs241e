package cs241e.assignments

import cs241e.mips.*
import Assembler.*
import ProgramRepresentation.*
import CodeBuilders.*
import cs241e.assignments.Reg.result

object mipsHelpers {
  // get element at index from array
  def getArrayElem(array: Variable, index: Variable) = block(
    binOp(
      getVar(array),
      plus,
      binOp(
        getVar(index),
        times,
        const(4)
      )
    ),
    LW(Reg.result, 0, Reg.result)
  )

  // get element at index from array
  def getArrayElem(array: Variable, index: Reg) = block(
    binOp(
      getVar(array),
      plus,
      binOp(
        getReg(index),
        times,
        const(4)
      )
    ),
    LW(Reg.result, 0, Reg.result)
  )

  // get element at index from array
  def getArrayElem(array: Reg, index: Reg) = block(
    binOp(
      getReg(array),
      plus,
      binOp(
        getReg(index),
        times,
        const(4)
      )
    ),
    LW(Reg.result, 0, Reg.result)
  )

  // stores constant in int in Reg.result
  def const(i: Int) = block(
    LIS(Reg.result),
    Word(encodeSigned(i))
  )

  // stores value of r in Reg.result
  def getReg(r: Reg) = ADD(Reg.result, Reg.zero, r)

  // stores value of v in Reg.result
  def getVar(v: Variable) = read(Reg.result, v)

  // stores value of v in r
  def getVar(r: Reg, v: Variable) = read(r, v)

  // prints r (mutates Reg.scratch)
  def printReg(r: Reg) = block(
    LIS(Reg.scratch),
    CPU.printAddr,
    SW(r, 0, Reg.scratch)
  )

  // assigns b to a
  def move(a: Reg, b: Reg) = ADD(a, b, Reg.zero)

  // Load immediate into r
  def li(r: Reg, imm: Word) = block(
    LIS(r),
    imm
  )

  // pushes r onto the stack (mutates Reg.scratch)
  def stackPush(r: Reg) = block(
    LIS(Reg.scratch),
    Word(encodeSigned(4)),
    SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
    SW(r, 0, Reg.stackPointer)
  )

  // gets top element from stack
  def stackTop(r: Reg) = LW(r, 0, Reg.stackPointer)

  // pops top element from stack (mutates Reg.scratch)
  def stackPop() = block(
    LIS(Reg.scratch),
    Word(encodeSigned(4)),
    ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch)
  )

  // pops and returns top element from stack (mutates Reg.scratch)
  def stackPop(r: Reg) = block(
    stackTop(r),
    LIS(Reg.scratch),
    Word(encodeSigned(4)),
    ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch)
  )
}
