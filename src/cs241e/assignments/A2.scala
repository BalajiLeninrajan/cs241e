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

import Assembler.*
import ProgramRepresentation.*
import Transformations.*
import cs241e.mips.*

object A2 {
  /* As part of Assignment 2, before implementing the methods in this file, first implement the methods in
   * Transformations.scala in the section for Assignment 2.
   */

  /* Registers 1 and 2 hold 32-bit integers (in two's-complement notation). Place the maximum of these integers
   * in register 3, and end execution.
   */
  lazy val maximum: Seq[Word] = {
    val code = Seq[Code](
      Assembler.ADD(Reg(3), Reg(1), Reg.zero),
      Assembler.SLT(Reg(1), Reg(1), Reg(2)),
      Assembler.BEQ(Reg(1), Reg.zero, 1),
      Assembler.ADD(Reg(3), Reg(2), Reg.zero),
      Assembler.JR(Reg.link)
    )
    eliminateLabels(code)
  }

  /* Registers 1 and 2 hold 32-bit integers (in unsigned integer notation). Place the maximum of these integers
   * in register 3, and end execution.
   */
  lazy val maximumUnsigned: Seq[Word] = {
    val code = Seq[Code](
      Assembler.ADD(Reg(3), Reg(1), Reg.zero),
      Assembler.SLTU(Reg(1), Reg(1), Reg(2)),
      Assembler.BEQ(Reg(1), Reg.zero, 1),
      Assembler.ADD(Reg(3), Reg(2), Reg.zero),
      Assembler.JR(Reg.link)
    )
    eliminateLabels(code)
  }

}
