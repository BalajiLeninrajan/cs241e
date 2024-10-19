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
import cs241e.Utils.*

import scala.annotation.tailrec
import scala.compiletime.ops.boolean

/** An assembler that generates machine language words representing MIPS
  * instructions.
  */

object Assembler {

  /* ## Assignment 1 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Given a sequence of bits, interpret it as an unsigned binary number and
    * return the number.
    *
    * Scala hint: Consult the Scala library documentation for classes such as
    * Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The purpose of this assignment is for *you* to write code that
    * encodes/decodes numbers in binary. Do not submit solutions that just call
    * functions in the Java/Scala standard library to do the conversion for you.
    */

  def decodeHelper(bits: Seq[Boolean]): Long = {
    bits.reverse.zipWithIndex.foldLeft(0L)((acc, unDecomposedItem) => {
      val (bit, index) = unDecomposedItem
      if (bit) acc + (1L << index) else acc
    })
  }

  def decodeUnsigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 0)
    require(bits.length <= 32)
    decodeHelper(bits)
  }

  /** Given a sequence of bits, interpret it as a signed two's-complement binary
    * number and return the number.
    */
  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 1)
    require(bits.length <= 32)
    val firstBit = bits(0)
    val decodedBitsButFirst = decodeHelper(bits.drop(1))
    if (!firstBit) decodedBitsButFirst
    else decodedBitsButFirst - (1L << (bits.length - 1))
  }

  /** Given a non-negative number `i`, encode it as an unsigned binary number
    * using the number of bits specified by `bits`.
    *
    * Scala hint: The `bits: Int = 32` specifies `32` as the default value of
    * bits. When calling this method, one can specify the number of bits
    * explicitly (e.g. `encodeUnsigned(42, 8)`), or leave it unspecified (e.g.
    * `encodeUnsigned(42)`), in which case the default value of 32 will be used.
    *
    * The length of the output sequence must be equal to `bits`.
    */

  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 0)
    require(bits <= 32)
    require(i >= 0)
    require(i < twoTo(bits))
    (0 until bits).map((offset) => (((i >> offset) & 1) == 1)).reverse
  }

  /** Given a number `i`, encode it as a signed two's-complement binary number
    * using the number of bits specified by `bits`.
    *
    * The length of the output sequence must be equal to `bits`.
    */
  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 1)
    require(bits <= 32)
    require(i >= -twoTo(bits - 1))
    require(i < twoTo(bits - 1))
    (0 until bits).map((offset) => (((i >> offset) & 1) == 1)).reverse
  }

  /* Before continuing Assignment 1, go to `A1.scala` and complete the methods there. Then return here and implement
   * the following.
   */

  /* Each of the following methods should encode the corresponding MIPS machine language instruction as a 32-bit `Word`.
   *
   * Hint: One way to create a word is from a sequence of 32 Booleans.
   * One way to create a sequence of Booleans is using Bits.
   *
   * For example:
   * `val fourBits = Seq(true, false, true, false)`
   * `val moreBits = Bits("0101")`
   * `val eightBits = fourBits ++ moreBits`
   * `val word = Word(eightBits ++ eightBits ++ eightBits ++ eightBits)`
   */

  /* Hint: You may implement additional helper methods if you wish to factor out common code. */

  def ADD(d: Reg, s: Reg, t: Reg = Reg.zero): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000100000")
  )
  def SUB(d: Reg, s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000100010")
  )
  def MULT(s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ Bits("0000000000011000")
  )
  def MULTU(s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ Bits("0000000000011001")
  )
  def DIV(s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ Bits("0000000000011010")
  )
  def DIVU(s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ Bits("0000000000011011")
  )
  def MFHI(d: Reg): Word = Word(
    Bits("0000000000000000")
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000010000")
  )
  def MFLO(d: Reg): Word = Word(
    Bits("0000000000000000")
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000010010")
  )
  def LIS(d: Reg): Word = Word(
    Bits("0000000000000000")
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000010100")
  )
  def LW(t: Reg, i: Int, s: Reg): Word = Word(
    Bits("100011")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeSigned(i, 16)
  )
  def SW(t: Reg, i: Int, s: Reg): Word = Word(
    Bits("101011")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeSigned(i, 16)
  )
  def SLT(d: Reg, s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000101010")
  )
  def SLTU(d: Reg, s: Reg, t: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeUnsigned(d.number, 5)
      ++ Bits("00000101011")
  )
  def BEQ(s: Reg, t: Reg, i: Int): Word = Word(
    Bits("000100")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeSigned(i, 16)
  )
  def BNE(s: Reg, t: Reg, i: Int): Word = Word(
    Bits("000101")
      ++ encodeUnsigned(s.number, 5)
      ++ encodeUnsigned(t.number, 5)
      ++ encodeSigned(i, 16)
  )
  def JR(s: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ Bits("000000000000000001000")
  )
  def JALR(s: Reg): Word = Word(
    Bits("000000")
      ++ encodeUnsigned(s.number, 5)
      ++ Bits("000000000000000001001")
  )
}
