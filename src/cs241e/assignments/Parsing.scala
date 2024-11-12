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

import cs241e.scanparse.*
import Grammars.*
import DFAs.*

import scala.collection.mutable

/** Parsers for general grammars. */

object Parsing {

  /** Parses the `input` sequence of `Token`s according to the `grammar` using
    * the Cocke-Younger-Kasami algorithm. Specifically, the `kind`s of the
    * `Token`s are considered as the terminals of the grammar, and the `lexeme`s
    * are not used for parsing but are preserved in the resulting parse tree.
    *
    * If the parse is ambiguous, returns an arbitrary one of the possible parse
    * trees.
    *
    * If the `input` is not in the language generated by the grammar, returns
    * `None`.
    */
  def parseCYK(grammar: Grammar, input: IndexedSeq[Token]): Option[Tree] = {

    /** The memoization table: if the string of symbols ABC derives the
      * substring of length `length` starting at position `from` of the `input`,
      * then the entry for (Seq("A", "B", "C"), from, length) contains the three
      * parse trees of A, B, and C. If a particular string of symbols does not
      * derive a given substring of the `input`, the corresponding table entry
      * is `None`.
      */
    val memo = mutable.Map[(Seq[String], Int, Int), Option[Seq[Tree]]]()

    /** If the string of symbols `lhs` derives the substring of length `length`
      * starting at position `from` of the `input`, returns a sequence of the
      * parse trees for the symbols in `lhs`.
      *
      * If `lhs` does not derive this substring of the input, returns `None`.
      */
    def recur(lhs: List[String], from: Int, length: Int): Option[Seq[Tree]] = {
      if (!memo.contains(lhs, from, length)) {
        memo((lhs, from, length)) = None
        if (lhs.isEmpty) { // case 1
          if (length == 0) memo((lhs, from, length)) = Option(Seq())
        } else if (grammar.terminals.contains(lhs.head)) { // case 2
          if (
            input(from).kind == lhs.head &&
            recur(lhs.tail, from + 1, length - 1).isDefined
          )
            memo((lhs, from, length)) =
              recur(lhs.tail, from + 1, length - 1).map(trees =>
                new Tree(input(from)) +: trees
              )
        } else if (lhs.tail.isEmpty) { // case 3
          memo((lhs, from, length)) = grammar
            .productionsExpanding(lhs.head)
            .flatMap(production =>
              recur(production.rhs.toList, from, length).map(trees =>
                Seq(new Tree(Token(lhs.head), trees))
              )
            )
            .headOption
        } else { // case 4
          memo((lhs, from, length)) = (0 until length)
            .flatMap(split =>
              grammar
                .productionsExpanding(lhs.head)
                .flatMap(production =>
                  recur(production.rhs.toList, from, split)
                    .flatMap(left =>
                      recur(lhs.tail, from + split, length - split)
                        .flatMap(right =>
                          Some(new Tree(Token(lhs.head), left) +: right)
                        )
                    )
                )
            )
            .headOption
        }
      }

      memo((lhs, from, length))
    }

    recur(List(grammar.start), 0, input.size).map(_.head)
  }

  /** Parses the `input` string of terminals according to the `grammar` using
    * Earley's algorithm. Returns `true` if the `input` string is in the
    * language generated by the `grammar`, `false` otherwise.
    *
    * Note: Optional, for bonus only.
    */
  def parseEarley(grammar: Grammar, input: IndexedSeq[String]): Boolean = {
    ???
  }
}
