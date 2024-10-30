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

import cs241e.scanparse.DFAs.*

object A7 {

  /** A sample DFA with alphabet {0,1} that recognizes binary integers that have
    * no useless (leading) zeroes.
    */
  val binaryNumbers = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "not0"),
    start = "start",
    accepting = Set("0", "not0"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "not0"
      case ("not0", _)    => "not0"
    }
  )

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no
    * useless (leading) zeroes and are not divisible by 3.
    */
  lazy val notDiv3 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "r0", "r1", "r2"),
    start = "start",
    accepting = Set("r1", "r2"),
    transition = {
      case ("start", '1') => "r1"
      case ("r0", '0')    => "r0"
      case ("r0", '1')    => "r1"
      case ("r1", '0')    => "r2"
      case ("r1", '1')    => "r0"
      case ("r2", '0')    => "r1"
      case ("r2", '1')    => "r2"
    }
  )

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no
    * useless (leading) zeroes and are not divisible by 2 or by 3.
    */
  lazy val notDiv23 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "00", "01", "02", "10", "11", "12"),
    start = "start",
    accepting = Set("11", "12"),
    transition = {
      case ("start", '1') => "11"
      case ("00", '0')    => "00"
      case ("00", '1')    => "11"
      case ("01", '0')    => "02"
      case ("01", '1')    => "10"
      case ("02", '0')    => "01"
      case ("02", '1')    => "12"
      case ("10", '0')    => "00"
      case ("10", '1')    => "11"
      case ("11", '0')    => "02"
      case ("11", '1')    => "10"
      case ("12", '0')    => "01"
      case ("12", '1')    => "12"
    }
  )

  /** A DFA that recognizes a decimal number between -128 and 127 inclusive,
    * with no useless zeroes. (Zeroes are required and only permitted if
    * removing them changes the meaning of the number.) The alphabet symbols are
    * {0,1,2,3,4,5,6,7,8,9,-}.
    */
  lazy val decimalNumber = DFA(
    alphabet = "-0123456789".toSet,
    states = Set("start", "zero", "0", "1", "2", "3", "4", "5", "6", "7"),
    start = "start",
    accepting = Set("zero", "1", "2", "3", "4", "5", "6", "7"),
    transition = {
      case ("start", '-')                           => "0"
      case ("start", '1')                           => "4"
      case ("start", '0')                           => "zero"
      case ("start", c) if ('2' to '9').contains(c) => "5"
      case ("0", '1')                               => "1"
      case ("0", c) if ('2' to '9').contains(c)     => "5"
      case ("1", c) if ('0' to '1').contains(c)     => "3"
      case ("1", '2')                               => "7"
      case ("1", c) if ('3' to '9').contains(c)     => "6"
      case ("2", c) if ('0' to '7').contains(c)     => "6"
      case ("3", c) if ('0' to '9').contains(c)     => "6"
      case ("4", c) if ('0' to '1').contains(c)     => "3"
      case ("4", '2')                               => "2"
      case ("4", c) if ('3' to '9').contains(c)     => "6"
      case ("5", c) if ('0' to '9').contains(c)     => "6"
      case ("7", c) if ('0' to '8').contains(c)     => "6"
    }
  )

  /** A DFA with alphabet {a, b, c} that recognizes any string that contains all
    * three letters in alphabetical order (i.e. "abc"), possibly interspersed
    * with more letters. For example, "acbac" and "cbacbacba" are in the
    * language, but not "acba".
    */
  lazy val abc = DFA(
    alphabet = "abc".toSet,
    states = Set("0", "1", "2", "3"),
    start = "0",
    accepting = Set("3"),
    transition = {
      case ("0", 'a')                   => "1"
      case ("0", c) if "bc".contains(c) => "0"
      case ("1", 'b')                   => "2"
      case ("1", c) if "ac".contains(c) => "1"
      case ("2", 'c')                   => "3"
      case ("2", c) if "ab".contains(c) => "2"
      case ("3", _)                     => "3"
    }
  )

  /** A DFA that recognizes any string from the alphabet {a,b,c} containing abc
    * as a substring.
    */
  lazy val abcSubstring = DFA(
    alphabet = "abc".toSet,
    states = Set("0", "1", "2", "3"),
    start = "0",
    accepting = Set("3"),
    transition = {
      case ("0", 'a')                   => "1"
      case ("0", c) if "bc".contains(c) => "0"
      case ("1", 'b')                   => "2"
      case ("1", 'a')                   => "1"
      case ("1", 'c')                   => "0"
      case ("2", 'a')                   => "1"
      case ("2", 'c')                   => "3"
      case ("2", 'b')                   => "0"
      case ("3", _)                     => "3"
    }
  )
}
