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

import ProgramRepresentation.*
import Assembler.*
import cs241e.mips.*
import cs241e.assignments.Reg.scratch
import cs241e.assignments.Reg.result
import cs241e.assignments.Reg.zero

/** Methods that generate `Code` for various higher-level language features. */

object CodeBuilders {
  /* ## Assignment 4 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Generates a binary operation that evaluates `e1` and `e2`, ensures that
    * the `Reg.result` from `e1` is in `Reg.scratch` and that the `Reg.result`
    * from `e2` is still in `Reg.result`, then executes `op`.
    *
    * Hint: Use a temporary variable and a `Scope`.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If
    * you need more than these registers, you may add new scratch registers to
    * Reg.scala. The generated code must not modify the values of any other
    * registers that are already listed in Reg.scala.
    */
  def binOp(e1: Code, op: Code, e2: Code): Code = {
    val t1 = Variable("tmp")
    Scope(
      Seq(t1),
      block(
        e1,
        write(t1, result),
        e2,
        read(scratch, t1),
        op
      )
    )
  }

  /* The following `Code`s are intended to be used as the `op` argument to `binOp`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, compute the corresponding arithmetic
   * operation, and leave the result of the operation in `Reg.result`.
   *
   * Assume that the operands are to be interpreted as signed two's-complement integers except in
   * `divideUnsigned` and `remainderUnsigned`.
   *
   * The generated code may modify the values of Reg.result and Reg.scratch. If you
   * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
   * must not modify the values of any other registers that are already listed in Reg.scala.
   */
  lazy val plus: Code = Assembler.ADD(result, scratch, result)
  lazy val minus: Code = Assembler.SUB(result, scratch, result)
  lazy val times: Code = block(
    Assembler.MULT(scratch, result),
    Assembler.MFLO(result)
  )
  lazy val divide: Code = block(
    Assembler.DIV(scratch, result),
    Assembler.MFLO(result)
  )
  lazy val remainder: Code = block(
    Assembler.DIV(scratch, result),
    Assembler.MFHI(result)
  )
  lazy val divideUnsigned: Code = block(
    Assembler.DIVU(scratch, result),
    Assembler.MFLO(result)
  )
  lazy val remainderUnsigned: Code = block(
    Assembler.DIVU(scratch, result),
    Assembler.MFHI(result)
  )

  /* The following `Code`s are intended to be used as the `comp` argument to `IfStmt`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, interpret them as two's-complement
   * signed integers (except `gtUnsignedCmp`), compare them, and branch to `label` if the comparison fails.
   *
   * The generated code may modify the values of Reg.result and Reg.scratch. If you
   * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
   * must not modify the values of any other registers that are already listed in Reg.scala.
   */
  def eqCmp(label: Label): Code = bne(scratch, result, label)
  def neCmp(label: Label): Code = beq(scratch, result, label)
  def ltCmp(label: Label): Code = block(
    SLT(scratch, scratch, result),
    beq(scratch, zero, label)
  )
  def gtCmp(label: Label): Code = block(
    SLT(scratch, result, scratch),
    beq(scratch, zero, label)
  )
  def leCmp(label: Label): Code = block(
    SLT(scratch, result, scratch),
    bne(scratch, zero, label)
  )
  def geCmp(label: Label): Code = block(
    SLT(scratch, scratch, result),
    bne(scratch, zero, label)
  )
  def gtUnsignedCmp(label: Label): Code = block(
    SLTU(scratch, result, scratch),
    beq(scratch, zero, label)
  )

  /** Generates code that evaluates `expr` to yield a memory address, then loads
    * the word from that address into `Reg.result`.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If
    * you need more than these registers, you may add new scratch registers to
    * Reg.scala. The generated code must not modify the values of any other
    * registers that are already listed in Reg.scala.
    */
  def deref(expr: Code): Code = block(
    expr,
    LW(result, 0, result)
  )

  /** Generates code that evaluates `target` to yield a memory address, then
    * evaluates `expr` to yield a value, then stores the value into the memory
    * address.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If
    * you need more than these registers, you may add new scratch registers to
    * Reg.scala. The generated code must not modify the values of any other
    * registers that are already listed in Reg.scala.
    */
  def assignToAddr(target: Code, expr: Code): Code = block(
    target,
    ADD(scratch, result, zero),
    expr,
    SW(result, 0, scratch)
  )

  /** Generates code that implements a while loop. The generated code should
    * evaluate `e1` and `e2`, compare them using `comp`, and if the comparison
    * succeeds, it should execute `body` and repeat the entire process from the
    * beginning.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If
    * you need more than these registers, you may add new scratch registers to
    * Reg.scala. The generated code must not modify the values of any other
    * registers that are already listed in Reg.scala.
    */
  def whileLoop(e1: Code, comp: Label => Code, e2: Code, body: Code): Code = {
    val whileLoopLabel = new Label("while loop")
    block(
      Define(whileLoopLabel),
      ifStmt(
        e1,
        comp,
        e2,
        block(
          body,
          beq(zero, zero, whileLoopLabel)
        )
      )
    )
  }

}
