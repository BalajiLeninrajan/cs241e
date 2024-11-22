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

import cs241e.*
import ProgramRepresentation.*
import CodeBuilders.*
import Typer.*
import mipsHelpers.*
import scanparse.Grammars.*
import cs241e.scanparse.DFAs.Token
import scala.runtime.stdLibPatches.language.`3.2`

/** A code generator that converts a Lacs parse tree into the intermediate
  * language developed in Assignment 1 to 6.
  */
object CodeGenerator {
  def generateProcedures(typedProcedures: TypedProcedures) = {

    /** Given a `procedureScope`, generates the `Code` implementing the
      * procedure, and assigns it to `procedureScope.procedure.code`.
      */
    def generateCode(procedureScope: ProcedureScope): Unit = {

      val symbolTable = typedProcedures.symbolTables(procedureScope)

      /** Generates the `Code` that implements `tree`.
        *
        * This method will be called from the outside only on `tree`s rooted at
        * nodes of kind "expras". However, if it calls itself recursively on
        * other kinds of nodes, it needs to be able to handle those node kinds
        * as well.
        */
      def recur(tree: Tree): Code = block(
        tree.production match
          case "expras expra SEMI expras" =>
            block(recur(tree.children.head), recur(tree.children.last))
          case "expras expra" => recur(tree.children.head)
          case "expra ID BECOMES expr" =>
            symbolTable(tree.children.head.lhs.lexeme) match
              case ps: ProcedureScope =>
                sys.error("proc where should be var")
              case TypedVariable(variable, tpe) =>
                assign(variable, recur(tree.children.last))
          case "expra expr" => recur(tree.children.head)
          case "expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE" => {
            val test = tree.children(2)
            val comp = test.children(1).lhs.kind match
              case "NE" => neCmp
              case "LT" => ltCmp
              case "LE" => leCmp
              case "GE" => geCmp
              case "GT" => gtCmp
              case "EQ" => eqCmp
            ifStmt(
              recur(test.children.head),
              comp,
              recur(test.children.last),
              recur(tree.children(5)),
              recur(tree.children(9))
            )
          }
          case "expr term" => recur(tree.children.head)
          case "expr expr PLUS term" =>
            binOp(
              recur(tree.children.head),
              plus,
              recur(tree.children.last)
            )
          case "expr expr MINUS term" =>
            binOp(
              recur(tree.children.head),
              minus,
              recur(tree.children.last)
            )
          case "term factor" => recur(tree.children.head)
          case "term term STAR factor" =>
            binOp(
              recur(tree.children.head),
              times,
              recur(tree.children.last)
            )
          case "term term SLASH factor" =>
            binOp(
              recur(tree.children.head),
              divide,
              recur(tree.children.last)
            )
          case "term term PCT factor" =>
            binOp(
              recur(tree.children.head),
              remainder,
              recur(tree.children.last)
            )
          case "factor ID" =>
            symbolTable(tree.children.head.lhs.lexeme) match
              case ps: ProcedureScope           => Closure(ps.procedure)
              case TypedVariable(variable, tpe) => getVar(variable)
          case "factor NUM" => const(tree.children.head.lhs.lexeme.toInt)
          case "factor LPAREN expr RPAREN" => recur(tree.children(1))
          case "factor factor LPAREN argsopt RPAREN" => {
            def argsHelper(tre: Tree): Seq[Code] = {
              tre.production match
                case "argsopt args" => argsHelper(tre.children.head)
                case "args expr COMMA args" =>
                  recur(tre.children.head) +: argsHelper(tre.children.last)
                case "args expr" => Seq(recur(tre.children.head))
                case _           => Seq()
            }
            val funCallTree = tree.children.head
            val args = argsHelper(tree.children(2))
            funCallTree.production match
              case "factor ID" =>
                symbolTable(funCallTree.children.head.lhs.lexeme) match
                  case TypedVariable(variable, tpe) => {
                    CallClosure(
                      getVar(variable),
                      args,
                      args.map(arg => new Variable("tmp"))
                    )
                  }
                  case proc: ProcedureScope =>
                    Call(proc.procedure, args)
              case _ =>
                CallClosure(
                  recur(funCallTree),
                  args,
                  args.map(arg => new Variable("tmp"))
                )
          }
      )

      /* Main body of generateCode. */
      procedureScope.procedure.code = Scope(
        procedureScope.variables.map(_.variable),
        recur(procedureScope.expras)
      )
    }

    /* Main body of generateProcedures. */

    typedProcedures.procedureScopes.foreach(generateCode)
    typedProcedures.procedureScopes.map(_.procedure)
  }
}
