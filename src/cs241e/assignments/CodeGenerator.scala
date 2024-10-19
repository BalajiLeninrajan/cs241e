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
import scanparse.Grammars.*

/** A code generator that converts a Lacs parse tree into the intermediate language developed in Assignment 1 to 6. */
object CodeGenerator {
  def generateProcedures(typedProcedures: TypedProcedures) = {

    /** Given a `procedureScope`, generates the `Code` implementing the procedure, and assigns it to
      * `procedureScope.procedure.code`.
      */
    def generateCode(procedureScope: ProcedureScope): Unit = {

      val symbolTable = typedProcedures.symbolTables(procedureScope)

      /** Generates the `Code` that implements `tree`.
        *
        * This method will be called from the outside only on `tree`s rooted at nodes of kind "expras".
        * However, if it calls itself recursively on other kinds of nodes, it needs to be able to handle
        * those node kinds as well.
        */
      def recur(tree: Tree): Code = ???

      /* Main body of generateCode. */
      procedureScope.procedure.code = Scope(procedureScope.variables.map(_.variable), recur(procedureScope.expras))
    }

    /* Main body of generateProcedures. */

    typedProcedures.procedureScopes.foreach(generateCode)
    typedProcedures.procedureScopes.map(_.procedure)
  }
}
