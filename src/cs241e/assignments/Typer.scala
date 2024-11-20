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
import cs241e.scanparse.Grammars.*

import scala.collection.mutable

/** Implementation of context-sensitive analysis for the Lacs language. */

object Typer {

  /** Representation of a Lacs type, which is either an Int or a function type
    * with parameter types and a return type.
    */
  sealed abstract class Type
  case object IntType extends Type
  case class FunctionType(parameterTypes: Seq[Type], returnType: Type)
      extends Type

  /** Given a `tree`, finds all descendants of the `tree` whose root node has
    * kind `lhsKind`. Does not search within the found subtrees for any nested
    * occurrences of additional descendants.
    *
    * For example, searching the root of a program tree with `lhsKind =
    * "procedure"` will return the trees all of the top-level procedures, but
    * not any procedures nested within them.
    */
  def collect(tree: Tree, lhsKind: String): Seq[Tree] =
    if (tree.lhs.kind == lhsKind) Seq(tree)
    else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))

  /** Given a tree that is either a "type" or contains exactly one "type" nested
    * within it, returns an instance of `Type` representing the corresponding
    * type.
    */
  def parseType(tree: Tree): Type = {
    val types = collect(tree, "type")
    require(types.size == 1)

    tree.production match {
      case "type INT" => IntType
      case "type LPAREN typesopt RPAREN ARROW type" =>
        FunctionType(
          parseTypesOpt(tree.children.tail.head),
          parseType(tree.children.last)
        )
      case _ => sys.error(s"parseType ${tree.production}")
    }
  }

  def parseTypesOpt(tree: Tree): Seq[Type] =
    tree.production match {
      case "typesopt types" => parseTypes(tree.children.head)
      case _                => Seq()
    }

  def parseTypes(tree: Tree): Seq[Type] =
    tree.production match {
      case "types type COMMA types" =>
        parseType(tree.children.head) +: parseTypes(tree.children.last)
      case "types type" => Seq(parseType(tree.children.head))
      case _            => sys.error(s"parseTypes: ${tree.production}")
    }

  /** A variable combined with its declared type. */
  case class TypedVariable(variable: Variable, tpe: Type)

  /** Create a new `Variable` given its `name` and type `tpe`. */
  def makeVariable(name: String, tpe: Type): Variable =
    new Variable(name, isPointer = (tpe != IntType))

  /** A `SymbolTable` maps each name to either a `TypedVariable` or a
    * `ProcedureScope`.
    */
  type SymbolTable = Map[String, TypedVariable | ProcedureScope]

  /** Given a tree containing subtrees rooted at "vardef", creates a
    * `TypedVariable` for each such tree.
    */
  def parseVarDefs(tree: Tree): Seq[TypedVariable] =
    collect(tree, "vardef").map { varDef =>
      TypedVariable(
        makeVariable(
          varDef.children.head.lhs.lexeme,
          parseType(varDef.children.last)
        ),
        parseType(varDef.children.last)
      )
    }

    /** Call `sys.error()` if any `String` occurs in `names` multiple times. */
  def checkDuplicates(names: Seq[String]): Unit = {
    val duplicates = names.diff(names.distinct)
    if (duplicates.nonEmpty) sys.error(s"Duplicate identifiers ${duplicates}")
  }

  /** A `ProcedureScope` holds the semantic information about a particular
    * procedure that is needed to type-check the body of the procedure,
    * including information coming from outer procedure(s) within which this
    * procedure may be nested.
    *
    * @param tree
    *   the tree defining the procedure (rooted at a "defdef")
    * @param outer
    *   the `ProcedureScope` of the outer procedure that immediately contains
    *   this one
    */
  class ProcedureScope(tree: Tree, outer: Option[ProcedureScope] = None) {
    assert(
      tree.production ==
        "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE"
    )
    val Seq(
      _,
      id,
      _,
      parmsopt,
      _,
      _,
      retTypeTree,
      _,
      _,
      vardefs,
      defdefs,
      expras,
      _
    ) = tree.children

    /** The name of the procedure. */
    val name: String = id.lhs.lexeme

    /** The parameters of the procedure. */
    val parms: Seq[TypedVariable] = parseVarDefs(parmsopt)

    /** The variables declared in the procedure. */
    val variables: Seq[TypedVariable] = parseVarDefs(vardefs)

    /** The declared return type of the procedure. */
    val returnType: Type = parseType(retTypeTree)

    /** The type of the procedure. */
    val tpe: FunctionType =
      FunctionType(parms.map(parm => parm.tpe), returnType)

    /** The new `Procedure` object that will represent this procedure. */
    val procedure: Procedure =
      new Procedure(
        name,
        parms.map(parm => parm.variable),
        outer.flatMap(value => Option(value.procedure))
      )

    /** The `ProcedureScope`s of the nested procedures that are immediately
      * nested within this procedure.
      *
      * Note: this `val` will recursively call `new ProcedureScope(...)`.
      */
    val subProcedures: Seq[ProcedureScope] =
      collect(defdefs, "defdef").map(defdef =>
        new ProcedureScope(defdef, Some(this))
      )

    /** The names of parameters, variables, and nested procedures that are newly
      * defined within this procedure (as opposed to being inherited from some
      * outer procedure).
      */
    val newNames: Seq[String] =
      parms.map(parm => parm.variable.name)
        ++ variables.map(variable => variable.variable.name)
        ++ subProcedures.map(subProcedure => subProcedure.name)
    checkDuplicates(newNames)

    /** Create and return a symbol table to be used when type-checking the body
      * of this procedure. It should contain all symbols (parameters, variables,
      * nested procedures) defined in this procedure, as well as those defined
      * in outer procedures within which this one is nested. Symbols defined in
      * this procedure override (shadow) those of outer procedures. The
      * `outerSymbolTable` parameter contains the symbol table of the enclosing
      * scope (either an outer procedure within which the current procedure is
      * nested, or, if the current procedure is a top-level procedure, a symbol
      * table containing the names of all of the top-level procedures).
      */
    def symbolTable(outerSymbolTable: SymbolTable): SymbolTable =
      outerSymbolTable
        ++ parms.map(parm => (parm.variable.name, parm))
        ++ variables.map(variable => (variable.variable.name, variable))
        ++ subProcedures.map(subProcedure => (subProcedure.name, subProcedure))

    /** Returns a sequence containing `this` `ProcedureScope` and the
      * `ProcedureScope`s for all procedures declared inside of this procedure,
      * including those nested recursively within other nested procedures.
      *
      * Scala hint: learn about the `flatMap` method in the Scala library. If
      * you are not familiar with flatMap, one place you can read about it is
      * here:
      * http://www.artima.com/pins1ed/working-with-lists.html#sec:higher-order-methods
      */
    def descendantScopes: Seq[ProcedureScope] =
      this +: subProcedures.flatMap(subProcedure =>
        subProcedure.descendantScopes
      )

    override def toString = s"ProcedureScope for $name"
  }

  /** Creates a map containing a symbol table for each procedure scope by
    * calling the scope's symbolTable method, passing in the symbol table of its
    * outer enclosing procedure (or the top level symbol table for a top level
    * procedure).
    */
  def createSymbolTables(
      topLevelProcedureScopes: Seq[ProcedureScope],
      topLevelSymbolTable: SymbolTable
  ): Map[ProcedureScope, SymbolTable] = {
    def recur(
        procedureScopes: Seq[ProcedureScope],
        outerSymbolTable: SymbolTable
    ): Map[ProcedureScope, SymbolTable] = {
      procedureScopes.flatMap { procedureScope =>
        val symbolTable = procedureScope.symbolTable(outerSymbolTable)
        Map(procedureScope -> symbolTable) ++ recur(
          procedureScope.subProcedures,
          symbolTable
        )
      }.toMap
    }
    recur(topLevelProcedureScopes, topLevelSymbolTable)
  }

  /** Checks that the body of a procedure satisfies the type-checking rules in
    * the Lacs language specification. Returns a `Map` that provides a `Type`
    * for each `Tree` that has a `Type` according to the language specification.
    */

  def typeCheck(
      scope: ProcedureScope,
      symbolTable: SymbolTable
  ): Map[Tree, Type] = {

    /** The map that will be returned containing the `Type` of each `Tree` that
      * has a `Type`.
      */
    val treeToType = mutable.Map[Tree, Type]()

    /** Calls `sys.error()` if `tpe1` and `tpe2` are not equal. If they are
      * equal, returns them.
      */
    def mustEqual(tpe1: Type, tpe2: Type): Type =
      if (tpe1 == tpe2) tpe1
      else sys.error(s"Type mismatch: expected $tpe2, got $tpe1")

    /** For a `tree` rooted at a node that has a `Type`, computes the `Type`,
      * adds it to `treeToType`, and returns it.
      *
      * Calls `sys.error()` if the `tree` does not conform to the typing rules
      * in the Lacs specification.
      */
    def typeOf(tree: Tree): Type = {
      def fun: Type = {
        tree.lhs.kind match {
          case "ID" =>
            symbolTable.getOrElse(tree.lhs.lexeme, None) match {
              case TypedVariable(variable, tpe) => tpe
              case proc: ProcedureScope         => proc.tpe
              case None => sys.error(s"Not in symbolTable ${tree.lhs.lexeme}")
            }
          case "factor" =>
            tree.production match {
              case "factor ID"  => typeOf(tree.children.last)
              case "factor NUM" => IntType
              case "factor LPAREN expr RPAREN" =>
                typeOf(tree.children.tail.head)
              case "factor factor LPAREN argsopt RPAREN" => {
                typeOf(tree.children.head) match {
                  case FunctionType(parameterTypes, returnType) => {
                    val args = collect(tree.children(2), "expr")
                    if (parameterTypes.length != args.length)
                      sys.error("factor args length")
                    parameterTypes.zip(args).foreach { case (tpe, tre) =>
                      mustEqual(tpe, typeOf(tre))
                    }
                    returnType
                  }
                  case _ => sys.error("factor not a function")
                }
              }
            }
          case "term" =>
            tree.production match {
              case "term factor" => typeOf(tree.children.last)
              case _ =>
                if (
                  mustEqual(
                    typeOf(tree.children.head),
                    typeOf(tree.children.last)
                  ) == IntType
                ) IntType
                else sys.error(s"term ${tree.production}")
            }
          case "expr" =>
            tree.production match {
              case "expr term" => typeOf(tree.children.head)
              case "expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE" =>
                if (
                  mustEqual(
                    typeOf(tree.children(2).children.head),
                    typeOf(tree.children(2).children.last)
                  ) == IntType
                ) mustEqual(typeOf(tree.children(5)), typeOf(tree.children(9)))
                else sys.error(s"expr test ${tree.children(2).production}")
              case _ =>
                mustEqual(
                  mustEqual(
                    typeOf(tree.children.head),
                    typeOf(tree.children.last)
                  ),
                  IntType
                )
            }
          case "expra" =>
            tree.production match {
              case "expra expr" => typeOf(tree.children.head)
              case _ => {
                symbolTable.getOrElse(
                  tree.children.head.lhs.lexeme,
                  None
                ) match {
                  case TypedVariable(variable, tpe) =>
                    mustEqual(
                      typeOf(tree.children.head),
                      typeOf(tree.children.last)
                    )
                  case proc: ProcedureScope => sys.error("ID is a procedure")
                  case None =>
                    sys.error(s"Not in symbolTable ${tree.lhs.lexeme}")
                }
              }
            }
          case "expras" =>
            tree.production match {
              case "expras expra" => typeOf(tree.children.last)
              case _ => {
                typeOf(tree.children.head)
                typeOf(tree.children.last)
              }
            }
          case "defdef" =>
            mustEqual(typeOf(tree.children(6)), typeOf(tree.children(11)))
        }
      }
      treeToType.getOrElseUpdate(tree, fun)
    }

    /* Check that the type of the expression returned from the procedure matches the declared type of the procedure. */
    mustEqual(scope.returnType, typeOf(scope.expras))

    Map() ++ treeToType
  }

  /** A data structure representing the result of context-sensitive analysis of
    * a whole Lacs program.
    *
    * @param procedureScopes
    *   the `ProcedureScopes` representing the semantic information about each
    *   procedure.
    * @param symbolTables
    *   a symbol table for each procedure in the program.
    * @param typeMap
    *   result of type-checking: provides a type for each tree node that
    *   represents an expression.
    */
  case class TypedProcedures(
      procedureScopes: Seq[ProcedureScope],
      symbolTables: ProcedureScope => SymbolTable,
      typeMap: PartialFunction[Tree, Type]
  ) {

    /** Output human-readable form of a parse tree (for debugging purposes)
      * annotated with the type information for tree nodes that have a type.
      */
    def showTree(tree: Tree, indent: Int = 0): String = {
      val typeString = typeMap.lift.apply(tree) match {
        case Some(tpe) => ": " + tpe
        case None      => ""
      }
      " " * indent + tree.lhs + typeString + "\n" +
        tree.children.map(ch => showTree(ch, indent + 1)).mkString
    }

    override def toString = procedureScopes
      .map { procedureScope =>
        procedureScope.toString + "\n" +
          "Symbol table:\n" +
          symbolTables(procedureScope).map { case (name, meaning) =>
            s" $name -> $meaning\n"
          }.mkString +
          "Procedure body with types:\n" +
          showTree(procedureScope.expras)
      }
      .mkString("\n")
  }

  /** Type-checks a Lacs program parse tree. Returns `TypedProcedures`, which
    * contains the `ProcedureScope`s representing the procedures, a map giving a
    * `SymbolTable` for each `ProcedureScope`, and a map giving the `Type` of
    * each `Tree` that has one.
    */
  def typeTree(tree: Tree): TypedProcedures = {
    assert(tree.production == "S BOF defdefs EOF")
    val defdefs = tree.children(1)

    val topLevelProcedureScopes = collect(defdefs, "defdef").map { defdef =>
      new ProcedureScope(defdef, None)
    }
    checkDuplicates(
      topLevelProcedureScopes.map(procedureScope => procedureScope.name)
    )
    val topLevelSymbolTable: SymbolTable =
      topLevelProcedureScopes.map { procedure =>
        (procedure.name -> procedure)
      }.toMap
    val symbolTables =
      createSymbolTables(topLevelProcedureScopes, topLevelSymbolTable)

    val allProcedureScopes = topLevelProcedureScopes.flatMap(procedureScope =>
      procedureScope.descendantScopes
    )

    val typeMap: Map[Tree, Type] = allProcedureScopes
      .flatMap(procedureScope =>
        typeCheck(procedureScope, symbolTables(procedureScope))
      )
      .toMap

    val mainProcedure = topLevelProcedureScopes.head
    if (mainProcedure.tpe != FunctionType(Seq(IntType, IntType), IntType))
      sys.error("The type of the main procedure must be (Int, Int)=>Int.")

    TypedProcedures(allProcedureScopes, symbolTables, typeMap)
  }
}
