package cs241e.assignments

import cs241e.mips.*
import Assembler.*
import ProgramRepresentation.*
import CodeBuilders.*

object mipsHelpers {
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

  def const(i: Int) = block(
    LIS(Reg.result),
    Word(encodeSigned(i))
  )

  def getReg(r: Reg) = ADD(Reg.result, Reg.zero, r)

  def getVar(v: Variable) = read(Reg.result, v)

  def printReg(r: Reg) = block(
    LIS(Reg.scratch),
    CPU.printAddr,
    SW(r, 0, Reg.scratch)
  )

  def move(a: Reg, b: Reg) = ADD(a, b, Reg.zero)

  def stackPush(r: Reg) = block(
    LIS(Reg.scratch),
    Word(encodeSigned(4)),
    SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
    SW(r, 0, Reg.stackPointer)
  )

  def stackTop(r: Reg) = LW(r, 0, Reg.stackPointer)

  def stackPop() = block(
    LIS(Reg.scratch),
    Word(encodeSigned(4)),
    ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch)
  )

  def stackPop(r: Reg) = block(
    stackTop(r),
    LIS(Reg.scratch),
    Word(encodeSigned(4)),
    ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch)
  )
}
