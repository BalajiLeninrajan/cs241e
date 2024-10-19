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
  /** A sample DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes. */
  val binaryNumbers = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "not0"),
    start = "start",
    accepting = Set("0", "not0"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "not0"
      case ("not0", _) => "not0"
    })

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
    * and are not divisible by 3.
    */
  lazy val notDiv3 = DFA(
    alphabet = "01".toSet,
    states = ???,
    start = ???,
    accepting = ???,
    transition = ???)

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
    * and are not divisible by 2 or by 3.
    */
  lazy val notDiv23 = DFA(
    alphabet = "01".toSet,
    states = ???,
    start = ???,
    accepting = ???,
    transition = ???)

  /** A DFA that recognizes a decimal number between -128 and 127 inclusive, with no useless zeroes.
    * (Zeroes are required and only permitted if removing them changes the meaning of the number.)
    * The alphabet symbols are {0,1,2,3,4,5,6,7,8,9,-}.
    */
  lazy val decimalNumber = DFA(
    alphabet = "-0123456789".toSet,
    states = ???,
    start = ???,
    accepting = ???,
    transition = ???
  )

  /** A DFA with alphabet {a, b, c} that recognizes any string that contains all three letters in
    * alphabetical order (i.e. "abc"), possibly interspersed with more letters. For example, "acbac"
    * and "cbacbacba" are in the language, but not "acba".
    */
  lazy val abc = DFA(
    alphabet = "abc".toSet,
    states = ???,
    start = ???,
    accepting = ???,
    transition = ???
  )

  /** A DFA that recognizes any string from the alphabet {a,b,c} containing abc as a substring. */
  lazy val abcSubstring = DFA(
    alphabet = "abc".toSet,
    states = ???,
    start = ???,
    accepting = ???,
    transition = ???
  )
}
