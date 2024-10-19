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
import CodeBuilders.*
import cs241e.assignments.A4.{const, getReg, printReg, getVar}
import cs241e.assignments.Reg.*
import cs241e.assignments.Assembler.*

object A5 {

  /** The code of `printInteger` from Assignment 4 encapsulated as a
    * `Procedure`. The procedure should have exactly one parameter, the integer
    * to be printed.
    */
  lazy val printProcedure: Procedure = {
    val parameter = new Variable("printIntegerCodeInput")
    val procedure = new Procedure("printProcedure", Seq(parameter))
    procedure.code = block(
      // You may add code here before and/or after `A4.printIntegerCode`. If you do not want any
      // code before or after, replace the ??? with an empty `block()`.
      read(Reg(1), parameter),
      A4.printIntegerCode
    )
    procedure
  }

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
    LW(result, 0, result)
  )

  /** This procedure will be executed with an array of 32-bit integers as in
    * Assignment 4. It should take two parameters: the first is the address of
    * the beginning of the array and the second is the number of elements in the
    * array. The procedure should call `printProcedure` for each integer in the
    * array in turn, to print all the integers, and return.
    *
    * Test this procedure by compiling it with `printProcedure` and running it
    * on various arrays.
    */
  lazy val printArray: Procedure = {
    val arr = new Variable("Array")
    val arrLen = new Variable("Lenght")
    val procedure = new Procedure("printArray", Seq(arr, arrLen))
    procedure.code = {
      val index = new Variable("index")
      Scope(
        Seq(index),
        block(
          assign(index, const(0)),
          whileLoop(
            getVar(index),
            ltCmp,
            getVar(arrLen),
            block(
              call(printProcedure, getArrayElem(arr, index)),
              assign(
                index,
                binOp(
                  getVar(index),
                  plus,
                  const(1)
                )
              )
            )
          )
        )
      )
    }
    procedure
  }

  /** This procedure will be executed with an array of 32-bit integers as in
    * Assignment 4. It should take two parameters: the first is the address of
    * the beginning of the array and the second is the number of elements in the
    * array.
    *
    * You may use multiple procedures if you wish. Generate them and return them
    * in a `Seq`. The tests will execute the first procedure in the sequence.
    *
    * The task is to determine the height of a binary tree and return it (in
    * `Reg.result`). Assume that every tree contains at least one node and hence
    * has a height of at least one. Each node of the tree is encoded in three
    * consecutive elements (words) of the array: a two's-complement integer
    * stored at the node, the node's left child, and the node's right child.
    * Each child is specified as the array index of the first element of the
    * child node. The integer -1 indicates that a node does not have a left or
    * right child. For example, the following tree:
    *
    * 77 / \ 22 -8 / \ -36 999
    *
    * could be encoded by following array:
    *
    * A[0] = 77 A[1] = 3 A[2] = 6 A[3] = 22 A[4] = -1 A[5] = -1 A[6] = -8 A[7] =
    * 9 A[8] = 12 A[9] = -36 A[10] = -1 A[11] = -1 A[12] = 999 A[13] = -1 A[14]
    * \= -1
    *
    * in which the root is encoded by the elements A[0], A[1] and A[2], the
    * root's left child is encoded by the elements A[3], A[4] and A[5], the
    * root's right child is encoded by the elements A[6], A[7] and A[8], the
    * root's left-most grandchild is encoded by the elements A[9], A[10] and
    * A[11], and the root's right-most grandchild is encoded by the elements
    * A[12], A[13] and A[14].
    *
    * This example tree has height 3.
    */
  lazy val treeHeight: Seq[Procedure] = {
    val array = new Variable("array")
    val length = new Variable("length")
    val currIndex = new Variable("current index")
    val treeHeight = new Procedure("treeHeight", Seq(array, length))
    val treeHelper = new Procedure("treeHelper", Seq(array, currIndex))

    treeHelper.code = {
      val leftIndex = new Variable("left Index")
      val rightIndex = new Variable("right Index")
      val leftLen = new Variable("left len")
      val rightLen = new Variable("right len")
      block(
        ifStmt(
          getVar(currIndex),
          eqCmp,
          const(-1),
          const(0),
          Scope(
            Seq(leftIndex, rightIndex, leftLen, rightLen),
            block(
              assign(currIndex, binOp(getVar(currIndex), plus, const(1))),
              assign(leftIndex, getArrayElem(array, currIndex)),
              assign(currIndex, binOp(getVar(currIndex), plus, const(1))),
              assign(rightIndex, getArrayElem(array, currIndex)),
              assign(
                leftLen,
                binOp(
                  const(1),
                  plus,
                  call(treeHelper, getVar(array), getVar(leftIndex))
                )
              ),
              assign(
                rightLen,
                binOp(
                  const(1),
                  plus,
                  call(treeHelper, getVar(array), getVar(rightIndex))
                )
              ),
              ifStmt(
                getVar(leftLen),
                gtCmp,
                getVar(rightLen),
                read(result, leftLen),
                read(result, rightLen)
              )
            )
          )
        )
      )
    }

    treeHeight.code = block(
      call(treeHelper, getVar(array), const(0))
    )
    /* You may, but are not required to, define and use more procedures in addition to `treeHeight`.
     * `treeHeight` must be the first procedure in the sequence that is returned.
     */
    Seq(treeHeight, treeHelper)
  }
}
