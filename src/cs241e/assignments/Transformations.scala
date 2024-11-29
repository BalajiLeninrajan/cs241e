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
import Assembler.*
import MemoryManagement.*
import cs241e.mips.*
import cs241e.assignments.Reg
import cs241e.Utils.*
import Debugger.*

import scala.collection.mutable
import scala.runtime.stdLibPatches.language.`3.1`
import scala.annotation.meta.param
import mipsHelpers.*

/** Implementations of various transformations on the `Code` objects defined in
  * `ProgramRepresentation.scala`. In general, the transformations successively
  * eliminate various types of `Code` objects by translating them into sequences
  * of simpler `Code`s, until only `Code`s directly representing machine
  * instructions are left.
  */

object Transformations {
  /* ############################################################### */
  /* ## Assignment 2 ############################################### */
  /* ############################################################### */

  /* Before doing Assignment 2, read the code in the first half of `ProgramRepresentation.scala`
   * (up to the `Block` case class) to get an idea of how we will represent programs using the various subclasses
   * of the `Code` class.
   *
   * Hint: You can navigate to the `Code` class by Ctrl-clicking on any use of the word `Code`.
   */

  /* Complete the implementation of the following method by replacing the `???`. */

  /** Given a sequence of `Code`s that may be one of `CodeWord`, `Define`,
    * `Use`, or `BeqBne`, resolve all of the labels, and output the
    * corresponding MIPS machine-language program as a sequence of `Word`s.
    *
    * Refer to `ProgramRepresentation.scala` for documentation of the meanings
    * of these `Code`s.
    *
    * If a label is defined multiple times or if a label is used but not
    * defined, call `sys.error()` with an appropriate error message.
    *
    * If the value of a label is so far away from a branch instruction that the
    * required offset cannot be encoded in the immediate bits of the branch
    * instruction, either call `sys.error()` with an appropriate error message
    * or throw an `IllegalArgumentException`. Note that the
    * `Assembler.encodeSigned` and `Assembler.decodeSigned` functions in the
    * provided handout code already contain `require` calls that will throw an
    * `IllegalArgumentException` if called with a number that cannot be encoded
    * in the specified number of bits.
    *
    * Scala hint: Consult the Scala library documentation for classes such as
    * Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The high-level structure of this method is given for you, but you should
    * learn about methods such as `foreach` and `flatMap` because you may want
    * to use them yourself in later assignments.
    */
  def eliminateLabels(code: Seq[Code]): Seq[Word] = {
    val symbolTable = mutable.Map[Label, Int]()

    /* First pass: fill in the `symbolTable` map with an address for each label. */
    def setLabels(): Unit = {
      var address = 0
      code.foreach {
        case Define(label) => {
          if (symbolTable.contains(label)) sys.error("Label already defined")
          symbolTable.addOne((label, address))
        }
        case _ => address += 4
      }
    }

    /* Second pass: replace each `Code` with an equivalent sequence of `Word`s. */
    def translate: Seq[Word] = {
      var location = 0
      code.flatMap {
        case Define(label) => {
          Seq()
        }
        case CodeWord(word) => {
          location += 4
          Seq(word)
        }
        case Use(label) => {
          location += 4
          if (!symbolTable.contains(label)) sys.error("Label never defined")
          Seq(Word(encodeUnsigned(symbolTable.apply(label).toLong)))
        }
        case BeqBne(bits, label) => {
          location += 4
          if (!symbolTable.contains(label)) sys.error("Label never defined")
          Seq(
            Word(
              bits :++ encodeSigned(
                (symbolTable.apply(label).toLong - location) >> 2,
                16
              )
            )
          )
        }
        case _ => impossible(s"Encountered unsupported code $code.")
      }
    }

    setLabels()
    translate
  }

  /** Links two sequences of code together to form a single program. */
  def link(codes1: Seq[Code], codes2: Seq[Code]): Seq[Code] = codes1 ++ codes2

  /** Remove all `Comment`s from a sequence of `Code`s.
    *
    * Assumes that the input sequence does not contain any `Code` types that are
    * defined after `Comment` in `ProgramRepresentation.scala`.
    */
  def eliminateComments(codes: Seq[Code]): Seq[Code] =
    codes diff codes.collect { case code: Comment => code }

  /** Eliminate all `Block`s from a tree of `Code` by flattening the tree into a
    * sequence of `Code`s other than `Block`s.
    *
    * Assumes that the input `code` tree does not contain any `Code` types that
    * are defined after `Block` in `ProgramRepresentation.scala`.
    */
  def eliminateBlocks(code: Code): Seq[Code] = code match {
    case Block(children) => children.flatMap(eliminateBlocks)
    case _               => Seq(code)
  }

  /** Transform a `Code` tree by applying the function `fun` to transform each
    * node of the tree of type `Code`.
    *
    * More specifically, at a given node `code`, first recursively call
    * `transformCodeTotal` to transform each of the children of the node. Then
    * apply `fun` on the resulting node whose children have been transformed.
    *
    * Note: `transformCodeTotal` should handle **all** the various possible
    * subclasses of `Code`, not only the ones that we have used so far.
    */
  def transformCodeTotal(code: Code, fun: Code => Code): Code = {

    /** Apply `transformCodeTotal` on all child code nodes of the argument code
      * node `code`.
      */
    def processChildren(code: Code): Code = code match {
      case Block(children) =>
        Block(children.map(child => transformCodeTotal(child, fun)))
      case Scope(variables, body) =>
        Scope(variables, transformCodeTotal(body, fun))
      case IfStmt(elseLabel, e1, comp, e2, thens, elses) =>
        IfStmt(
          elseLabel,
          transformCodeTotal(e1, fun),
          transformCodeTotal(comp, fun),
          transformCodeTotal(e2, fun),
          transformCodeTotal(thens, fun),
          transformCodeTotal(elses, fun)
        )
      case Call(procedure, args, isTail) =>
        Call(procedure, args.map(arg => transformCodeTotal(arg, fun)), isTail)
      case CallClosure(closure, args, params, isTail) =>
        CallClosure(
          transformCodeTotal(closure, fun),
          args.map(arg => transformCodeTotal(arg, fun)),
          params,
          isTail
        )
      case _ => code
    }

    fun(processChildren(code))
  }

  /** A adaptation of `transformCodeTotal` to transform code trees with a
    * partial function. If the partial function is not defined for a particular
    * tree node, that tree node is left unmodified.
    *
    * Scala Hint: Read
    * https://www.scala-lang.org/api/current/scala/PartialFunction.html for an
    * explanation of `PartialFunction`.
    */
  def transformCode(code: Code, fun: PartialFunction[Code, Code]): Code = {
    transformCodeTotal(
      code,
      code => if (fun.isDefinedAt(code)) fun(code) else code
    )
  }

  /* ############################################################### */
  /* ## Assignment 3 ############################################### */
  /* ############################################################### */

  /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with
    * machine language code for reading or writing the relevant variable.
    *
    * To do this, we need an activation record or `frame` containing the
    * `Variable`s used in `code` that determines the address in memory where
    * each `Variable` is stored, as an offset from the address in
    * `Reg.framePointer`. See the definition of the `Chunk` class in
    * `MemoryManagement.scala` for details.
    *
    * Hint: the `transformCode` method is very helpful for transforming `Code`
    * in general, and for eliminating `VarAccess`es specifically.
    *
    * Scala Hint: {case va: VarAccess => ???} defines a `PartialFunction` that
    * is defined for `VarAccess`es but is not defined for any other type of
    * `Code`.
    *
    * Assumes that the input sequence does not contain any `Code` types that are
    * defined after `VarAccess` in `ProgramRepresentation.scala`.
    *
    * The code generated to implement a variable access may modify the value of
    * Reg.scratch. If the access is a read, it may also of course modify the
    * target register. If you need more than these registers, you may add new
    * scratch registers to Reg.scala. The generated code must not modify the
    * values of any other registers that are already listed in Reg.scala.
    */
  def eliminateVarAccessesA3(code: Code, frame: Chunk): Code = {
    def fun: PartialFunction[Code, Code] = { case va: VarAccess =>
      if (va.read) {
        frame.load(Reg.framePointer, va.register, va.variable)
      } else {
        frame.store(Reg.framePointer, va.variable, va.register)
      }
    }

    transformCode(code, fun)
  }

  /** Given a `body` of `Code` and a `frame` of variables that it uses,
    * generates code that allocates space for the `frame` on the stack and sets
    * `Reg.framePointer` to point to it, followed by the `body`, followed by
    * code to free the space for the `frame` from the stack.
    */
  def allocateFrameOnStack(body: Code, frame: Chunk): Code =
    block(
      Stack.allocate(frame),
      ADD(Reg.framePointer, Reg.stackPointer, Reg.zero),
      body,
      Stack.pop,
      ADD(Reg.framePointer, Reg.stackPointer, Reg.zero)
    )

  /** A bundle of machine language code in the form of a sequence of `Word`s and
    * a `debugTable` for the `Debugger`.
    */
  case class MachineCode(words: Seq[Word], debugTable: DebugTable)

  /** Given a `Code` tree containing only `Code` types that are defined before
    * `Block` in `ProgramRepresentation.scala`, successively eliminates all
    * `Code` types to yield just a sequence of `Word`s representing the
    * equivalent program in machine language. In addition, generates a
    * `DebugTable` for that program.
    */
  def toMachineCode(code: Code): MachineCode = {
    val code2 = eliminateBlocks(code)
    val code3 = eliminateComments(code2)
    val code4 = eliminateLabels(code3)
    val debugTable = createDebugTable(code2)
    MachineCode(code4, debugTable)
  }

  /** Given a `Code` tree that may contain any `Code` types defined in
    * `ProgramRepresentation.scala` up to and including `VarAccess`,
    * successively eliminates all `Code` types to yield just a sequence of
    * `Word`s representing the equivalent program in machine language. In
    * addition, generates a `DebugTable` for that program.
    *
    * Requires a sequence of all `Variable`s that are accessed within `code`.
    * The generated machine language code will allocate space for the variables
    * on the stack and free it at the end.
    */
  def compilerA3(code: Code, variables: Seq[Variable]): MachineCode = {
    val frame = Chunk(variables)
    val code1 = eliminateVarAccessesA3(code, frame)
    val code2 = allocateFrameOnStack(code1, frame)
    toMachineCode(block(code2, JR(Reg.link)))
  }

  /* ############################################################### */
  /* ## Assignment 4 ############################################### */
  /* ############################################################### */

  /** Eliminate all `Scope`s from a tree of `Code` by simply returning the
    * `code` field for each one.
    *
    * Return a pair of the resulting code and a sequence of the temporary
    * `Variable`s extracted from the `Scope`s.
    *
    * Assumes that the input `code` tree does not contain any `Code` types that
    * are defined after `Scope` in `ProgramRepresentation.scala`.
    *
    * Hint: Use `transformCode`.
    */
  def eliminateScopes(code: Code): (Code, Seq[Variable]) = {
    var scopeVariables = Seq[Variable]();
    def eliminateScopeHelper: PartialFunction[Code, Code] = {
      case scope: Scope => {
        val (scopeCode, subScopeVariables) = eliminateScopes(scope.code)
        scopeVariables = scopeVariables ++ scope.variables ++ subScopeVariables
        scopeCode
      }
    }

    (transformCode(code, eliminateScopeHelper), scopeVariables)
  }

  /** Eliminate all `IfStmt`s from a tree of `Code` by translating them to
    * simpler pieces of `Code`.
    *
    * Assumes that the input `code` tree does not contain any `Code` types that
    * are defined after `IfStmt` in `ProgramRepresentation.scala`.
    *
    * The code generated to implement an if statement may modify the values of
    * Reg.result and Reg.scratch. If you need more than these registers, you may
    * add new scratch registers to Reg.scala. The generated code must not modify
    * the values of any other registers that are already listed in Reg.scala.
    */
  def eliminateIfStmts(code: Code): Code = {
    def fun: PartialFunction[Code, Code] = {
      case stmt: IfStmt => {
        val endLabel = new Label("end")
        block(
          binOp(
            eliminateIfStmts(stmt.e1),
            eliminateIfStmts(stmt.comp),
            eliminateIfStmts(stmt.e2)
          ),
          eliminateIfStmts(stmt.thens),
          beq(Reg.zero, Reg.zero, endLabel),
          Define(stmt.elseLabel),
          eliminateIfStmts(stmt.elses),
          Define(endLabel)
        )
      }
    }

    transformCode(code, fun)
  }

  def compilerA4(code: Code): MachineCode = {
    val code1 = eliminateIfStmts(code)
    val (code2, variables) = eliminateScopes(code1)
    assert(variables.distinct == variables)
    compilerA3(code2, variables)
  }

  /* ############################################################### */
  /* ## Assignment 5 ############################################### */
  /* ############################################################### */

  /** For each `Variable` in the sequence `params`, creates a new variable
    * (intended to be used as a temporary variable). Returns a sequence of
    * temporary variables of the same length as the input sequence `params`. For
    * each `Variable` in `params`, if the `isPointer` flag of the variable is
    * set to true, then the `isPointer` flag of the temporary variable at the
    * same position in the returned sequence will also be set to true.
    */
  def createTempVars(params: Seq[Variable]): Seq[Variable] =
    params.map(param => new Variable("temp", param.isPointer))

  /** Given a set of `keys` for a map and a `function`, applies the function to
    * each key, and stores the result in a `Map` from `keys` to the `function`
    * values.
    */
  def makeMap[A, B](keys: Seq[A], function: A => B): Map[A, B] =
    keys.map(key => (key, function(key))).toMap

  /** Given a sequence of `Procedure`s, compiles the procedures to machine
    * language. The first procedure in the sequence is considered the main
    * procedure that should be executed first in the program. This is achieved
    * by adding an extra `startProcedure` that calls the main procedure with the
    * values of registers $1 and $2 as arguments. The main procedure must have
    * exactly two parameters to receive these values.
    */
  def compilerA5(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters of each procedure. */
    val paramChunks: Map[Procedure, Chunk] =
      makeMap(procedures, procedure => Chunk(procedure.parameters))

    /** Compile a single procedure to machine language. */
    def compileProcedure(currentProcedure: Procedure): Code = {

      /** Eliminate all `Call`s from a tree of `Code` by translating them to
        * simpler pieces of `Code`.
        *
        * The general strategy for passing arguments is as follows:
        *   - create temporary variables, one for each argument
        *   - evaluate the arguments, storing them in the temporary variables
        *   - allocate memory for the parameter `Chunk`
        *   - copy the values of the temporary variables to the parameter
        *     `Chunk`
        *
        * Assumes that the input `code` tree does not contain any `Code` types
        * that are defined after `Call` in `ProgramRepresentation.scala`.
        */
      def eliminateCalls(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = { case call: Call =>
          // Note: `caller` does not need to be used in the solution for `eliminateCalls`.
          // It is made explicit here only to distinguish it from `callee`.
          val caller = currentProcedure
          val callee = call.procedure
          val tempVars = createTempVars(callee.parameters)
          val paramChunk = paramChunks(callee)
          Scope(
            tempVars,
            block(
              Block(
                tempVars
                  .zip(call.arguments)
                  .map((variable, code) => assign(variable, code))
              ),
              Stack.allocate(paramChunk),
              Block(
                tempVars
                  .zip(callee.parameters)
                  .map((tempVar, parameter) =>
                    block(
                      read(Reg.scratch, tempVar),
                      paramChunk
                        .store(Reg.result, parameter, Reg.scratch)
                    )
                  )
              ),
              LIS(Reg.targetPC),
              Use(callee.label),
              JALR(Reg.targetPC)
            )
          )
        }
        transformCode(code, fun)
      }

      /* First part of main body of compileProcedure. */
      val code1 = eliminateCalls(currentProcedure.code)
      val code2 = eliminateIfStmts(code1)
      val (code3, variables) = eliminateScopes(code2)
      assert(variables.distinct == variables)

      val frame = Chunk(
        variables ++
          Seq(
            currentProcedure.savedPC,
            currentProcedure.dynamicLink,
            currentProcedure.paramPtr
          )
      )

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * The prologue assumes that when the procedure is called, `Reg.result`
        * contains the address of the parameter chunk for the procedure.
        *
        * The prologue:
        *   - saves the address of the parameter chunk in order to later store
        *     it into the `paramPtr` variable in the procedure `frame`
        *   - allocates space for the procedure `frame` on the stack
        *   - saves the caller's value of `Reg.framePointer` in the
        *     `dynamicLink` variable of the procedure `frame`
        *   - sets `Reg.framePointer` to the address of the newly-allocated
        *     `frame` for the callee
        *   - saves `Reg.link` in the `savedPC` variable of the `frame`
        *   - stores the saved address of the parameter chunk into the
        *     `paramPtr` variable in the procedure `frame`
        *
        * The epilogue:
        *   - restores `Reg.link` and `Reg.framePointer` from the current frame
        *   - pops the callee's parameter chunk and `frame` off the stack
        *   - returns control to the caller
        *
        * Warning: this method transforms code after `eliminateVarAccesses` has
        * been called. Therefore, this method should not introduce any new
        * `VarAccess`es into the code (by calling `read` or `write`).
        *
        * Hint: this method can use the `frame` from its enclosing method
        * `compileProcedure`.
        */
      def addEntryExit(code: Code): Code = {
        val enter = block(
          ADD(Reg.savedParamPtr, Reg.zero, Reg.result),
          Stack.allocate(frame),
          frame
            .store(Reg.result, currentProcedure.dynamicLink, Reg.framePointer),
          ADD(Reg.framePointer, Reg.result, Reg.zero),
          frame.store(Reg.result, currentProcedure.savedPC, Reg.link),
          frame.store(Reg.result, currentProcedure.paramPtr, Reg.savedParamPtr)
        )
        val exit = block(
          frame.load(
            Reg.framePointer,
            Reg.link,
            currentProcedure.savedPC
          ),
          frame.load(
            Reg.framePointer,
            Reg.framePointer,
            currentProcedure.dynamicLink
          ),
          Stack.pop,
          Stack.pop,
          JR(Reg.link)
        )
        block(Define(currentProcedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them
        * with machine language code for reading or writing the relevant
        * variable.
        *
        * In contrast to Assignment 3, this method needs to handle accesses not
        * only to variables, but also to the parameters of the current
        * procedure.
        *
        * Assumes that the input sequence does not contain any `Code` types that
        * are defined after `VarAccess` in `ProgramRepresentation.scala`.
        *
        * The code generated to implement a variable access may modify the value
        * of Reg.scratch. If the access is a read, it may also of course modify
        * the target register. If you need more than these registers, you may
        * add new scratch registers to Reg.scala. The generated code must not
        * modify the values of any other registers that are already listed in
        * Reg.scala.
        *
        * Hint: this method can use the `frame` from its enclosing procedure
        * `compileProcedure`.
        */
      def eliminateVarAccessesA5(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = { case va: VarAccess =>
          if (currentProcedure.parameters.contains(va.variable)) {
            if (va.read) {
              block(
                frame.load(
                  Reg.framePointer,
                  Reg.scratch,
                  currentProcedure.paramPtr
                ),
                paramChunks(currentProcedure).load(
                  Reg.scratch,
                  va.register,
                  va.variable
                )
              )
            } else {
              block(
                frame
                  .load(
                    Reg.framePointer,
                    Reg.scratch,
                    currentProcedure.paramPtr
                  ),
                paramChunks(currentProcedure).store(
                  Reg.scratch,
                  va.variable,
                  va.register
                )
              )
            }
          } else {
            if (va.read) {
              frame.load(Reg.framePointer, va.register, va.variable)
            } else {
              frame.store(Reg.framePointer, va.variable, va.register)
            }
          }
        }
        transformCode(code, fun)
      }

      /* Rest of main body of compileProcedure. */

      val code4 = eliminateVarAccessesA5(code3)
      addEntryExit(code4)
    }

    /* Main body of compilerA5. */

    val code = block(
      Stack.allocate(
        Chunk(procedures.head.parameters)
      ), // allocate parameter chunk for start procedure
      Block(procedures.map(compileProcedure))
    )
    toMachineCode(code)
  }

  def startProcedure(mainProcedure: Procedure): Procedure = {
    val ret = new Procedure("start", Seq())
    ret.code = Call(
      mainProcedure,
      Seq(ADD(Reg.result, Reg.input1), ADD(Reg.result, Reg.input2))
    )
    ret
  }

  /* ############################################################### */
  /* ## Assignment 6 ############################################### */
  /* ############################################################### */

  val closureCode = new Variable("closure code")
  val closureEnvironment = new Variable("closure environment", isPointer = true)

  /** A chunk representing a closure consisting of:
    *   - the address of the code of the closure
    *   - the address of the frame of the enclosing environment of the closure,
    *     which will become the static link when the closure is invoked
    */
  lazy val closureChunk = Chunk(Seq(closureCode, closureEnvironment))

  /** Given a sequence of `Procedure`s, compiles the procedures to machine
    * language. The first procedure in the sequence is considered the main
    * procedure that should be executed first in the program. This is achieved
    * by adding an extra `startProcedure` that calls the main procedure with the
    * values of registers $1 and $2 as arguments. The main procedure must have
    * exactly two parameters to receive these values.
    */
  def compilerA6(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters and static link of each procedure.
      */
    val paramChunks: Map[Procedure, Chunk] =
      makeMap(
        procedures,
        procedure => Chunk(procedure.parameters :+ procedure.staticLink)
      )

    /** The set of procedures whose frame needs to be allocated on the heap
      * (instead of on the stack). This includes:
      *   - every procedure that is ever made into a closure
      *   - recursively, every enclosing procedure that contains an inner
      *     procedure 2ested within it whose frame is allocated on the heap
      */
    var frameOnHeap: Set[Procedure] = Set()
    def addToFrameOnHeap(proc: Procedure): Unit = {
      var cur = Option(proc)
      while (cur.isDefined) {
        frameOnHeap += cur.get
        cur = cur.get.outer
      }
    }

    /** The first phase of compilation: performs the transformations up to
      * eliminateScopes so that the full set of variables of the procedure is
      * known, and so that a `Chunk` can be created for the frame of the
      * procedure. Since the second phase requires a frame for every procedure,
      * the first phase must be completed for all the procedures before the
      * second phase can begin. Returns the code for the procedure and the
      * `Chunk` for the procedure's frame.
      */
    def phaseOne(currentProcedure: Procedure): (Code, Chunk) = {

      /** Generates the code that computes the value for the static link in a
        * call to callee. Specifically, the static link should be the address of
        * the frame in the current chain of static links that corresponds to the
        * procedure that directly encloses callee. This address should be placed
        * in `Reg.result`.
        *
        * When `callee` is a top-level procedure with no outer enclosing
        * procedure, returns code that places the zero word in `Reg.result`.
        */
      def computeStaticLink(callee: Procedure): Code = {
        if (callee.depth == 0) const(0)
        val caller = currentProcedure
        val n = caller.depth - callee.depth + 1
        n match {
          case 0 => getReg(Reg.framePointer)
          case 1 => getVar(caller.staticLink)
          case _ => {
            getVar(
              (1 until n)
                .foldLeft(caller) { (x, _) => x.outer.get }
                .staticLink
            )
          }
        }
      }

      /** Eliminate all `Call`s and `CallClosure`s from a tree of `Code` by
        * translating them to simpler pieces of `Code`.
        *
        * Assumes that the input `code` tree does not contain any `Code` types
        * that are defined after `CallClosure` in `ProgramRepresentation.scala`.
        */
      def eliminateCalls(code: Code): Code = {
        val caller = currentProcedure

        def fun: PartialFunction[Code, Code] = {
          case call: Call => {
            val callee = call.procedure
            val paramChunkAddr = new Variable(
              "Address of allocated param chunk"
            )
            val tempVars = createTempVars(callee.parameters)
            val paramChunk = paramChunks(callee)
            Scope(
              tempVars :+ paramChunkAddr,
              block(
                Block(
                  tempVars
                    .zip(call.arguments)
                    .map((variable, code) => assign(variable, code))
                ),
                if (frameOnHeap(callee))
                  heap.allocate(paramChunk)
                else
                  Stack.allocate(paramChunk),
                Block(
                  tempVars
                    .zip(callee.parameters)
                    .map((tempVar, parameter) =>
                      block(
                        read(Reg.scratch, tempVar),
                        paramChunk
                          .store(Reg.result, parameter, Reg.scratch)
                      )
                    )
                ),
                write(paramChunkAddr, Reg.result),
                computeStaticLink(callee),
                read(Reg.scratch, paramChunkAddr),
                paramChunk
                  .store(Reg.scratch, callee.staticLink, Reg.result),
                read(Reg.result, paramChunkAddr),
                LIS(Reg.targetPC),
                Use(callee.label),
                JALR(Reg.targetPC)
              )
            )
          }
          case closureCall: CallClosure => {
            val closureStaticLink = new Variable("static link of closure")
            val closureChunkAddr = new Variable("Address of Closure Chunk")
            val closureParamChunkAddr = new Variable(
              "Address of Closure Param Chunk"
            )
            val tempVars = createTempVars(
              closureCall.parameters :+ closureStaticLink
            )
            val closureParamChunk = Chunk(
              closureCall.parameters :+ closureStaticLink
            )
            Scope(
              tempVars :+ closureChunkAddr :+ closureParamChunkAddr,
              block(
                Block(
                  closureCall.arguments
                    .zip(tempVars)
                    .map((code, variable) => assign(variable, code))
                ),
                closureCall.closure,
                write(closureChunkAddr, Reg.result),
                assign(
                  tempVars.last,
                  closureChunk.load(Reg.result, Reg.result, closureEnvironment)
                ),
                heap.allocate(closureParamChunk),
                write(closureParamChunkAddr, Reg.result),
                Block(
                  closureCall.parameters
                    .zip(tempVars)
                    .map((parameter, tempVar) =>
                      block(
                        read(Reg.scratch, tempVar),
                        closureParamChunk
                          .store(Reg.result, parameter, Reg.scratch)
                      )
                    )
                ),
                read(Reg.scratch, tempVars.last),
                closureParamChunk
                  .store(Reg.result, closureStaticLink, Reg.scratch),
                read(Reg.result, closureChunkAddr),
                closureChunk.load(Reg.result, Reg.targetPC, closureCode),
                read(Reg.result, closureParamChunkAddr),
                JALR(Reg.targetPC)
              )
            )
          }
        }
        transformCode(code, fun)
      }

      /** Eliminate all `Closure`s from a tree of `Code` by translating them to
        * simpler pieces of `Code`.
        *
        * As given, this method just returns the `code` unchanged. When you
        * implement handling of closures in Assignment 6, you will change the
        * method body to actually eliminate `Closure`s.
        */
      def eliminateClosures(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case closure: Closure => {
            addToFrameOnHeap(currentProcedure)
            val closureChunkAddr = new Variable(
              "Address of newly created closure"
            )
            Scope(
              Seq(closureChunkAddr),
              block(
                heap.allocate(closureChunk),
                write(closureChunkAddr, Reg.result),
                LIS(Reg.result),
                Use(closure.procedure.label),
                read(Reg.scratch, closureChunkAddr),
                closureChunk.store(Reg.scratch, closureCode, Reg.result),
                computeStaticLink(closure.procedure),
                read(Reg.scratch, closureChunkAddr),
                closureChunk.store(Reg.scratch, closureEnvironment, Reg.result),
                read(Reg.result, closureChunkAddr)
              )
            )
          }
        }
        transformCode(code, fun)
      }

      /** Find `Call`s that appear in tail position in their containing
        * procedure. Replace each one with the same `Call` but with `isTail` set
        * to `true`.
        *
        * Hint: If a call in tail position is to a callee procedure nested
        * within the caller procedure, is it safe to do a tail call?
        *
        * As given, this method just returns the `code` unchanged. When you
        * implement handling of tail calls in Assignment 6, you will change the
        * method body to actually detect tail calls.
        */
      def detectTailCalls(code: Code): Code = code

      /* Main body of phaseOne. */

      val code1 = eliminateClosures(currentProcedure.code)
      val code2 = detectTailCalls(code1)
      val code3 = eliminateCalls(code2)
      val code4 = eliminateIfStmts(code3)
      val (code5, variables) = eliminateScopes(code4)
      assert(variables.distinct == variables)

      val frame = Chunk(
        variables ++
          Seq(
            currentProcedure.savedPC,
            currentProcedure.dynamicLink,
            currentProcedure.paramPtr
          )
      )

      (code5, frame)
    }

    val phaseOneResults: Map[Procedure, (Code, Chunk)] =
      makeMap(procedures, phaseOne)

    def phaseTwo(currentProcedure: Procedure): Code = {
      val (code, frame) = phaseOneResults(currentProcedure)

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * Hint: The implementation of this method starts out the same as in
        * Assignment 5, and you can copy your code from there. When you
        * implement closures, you need to modify it so that the frame and
        * parameters are on the heap if `frameOnHeap(currentProcedure)` is true.
        *
        * The prologue assumes that when the procedure is called, `Reg.result`
        * contains the address of the parameter chunk for the procedure.
        *
        * The prologue:
        *   - saves the address of the parameter chunk to stores it into the
        *     `paramPtr` variable in the procedure `frame`
        *   - allocates space for the procedure `frame` on the stack or heap
        *   - saves the caller's value of `Reg.framePointer` in the
        *     `dynamicLink` variable of the procedure `frame`
        *   - sets `Reg.framePointer` to the address of the newly-allocated
        *     `frame` for the callee
        *   - saves `Reg.link` in the `savedPC` variable of the `frame`
        *
        * The epilogue:
        *   - restores `Reg.link` and `Reg.framePointer` from the current frame
        *   - pops the callee's parameter chunk and `frame` off the stack if
        *     they are allocated on the stack
        *   - returns control to the caller
        *
        * Warning: this method transforms code after `eliminateVarAccesses` has
        * been called. Therefore, this method should not introduce any new
        * `VarAccess`es into the code (by calling `read` or `write`).
        */
      def addEntryExit(code: Code): Code = {
        val enter = block(
          ADD(Reg.savedParamPtr, Reg.zero, Reg.result),
          if (frameOnHeap(currentProcedure))
            heap.allocate(frame)
          else
            Stack.allocate(frame),
          frame
            .store(Reg.result, currentProcedure.dynamicLink, Reg.framePointer),
          ADD(Reg.framePointer, Reg.result, Reg.zero),
          frame.store(Reg.result, currentProcedure.savedPC, Reg.link),
          frame.store(Reg.result, currentProcedure.paramPtr, Reg.savedParamPtr)
        )
        val exit = block(
          frame.load(
            Reg.framePointer,
            Reg.link,
            currentProcedure.savedPC
          ),
          frame.load(
            Reg.framePointer,
            Reg.framePointer,
            currentProcedure.dynamicLink
          ),
          if (!frameOnHeap(currentProcedure))
            block(Stack.pop, Stack.pop)
          else block(),
          JR(Reg.link)
        )
        block(Define(currentProcedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them
        * with machine language code for reading or writing the relevant
        * variable.
        *
        * In contrast to Assignment 5, this method handles accesses to variables
        * (and parameters) outside the currently executing procedure, but in one
        * of the outer procedures within which the current procedure is nested.
        * To do this, look up the chain of static links to find the frame (and
        * its associated parameters) of the procedure in which the variable is
        * defined.
        *
        * Assumes that the input sequence does not contain any `Code` types that
        * are defined after `VarAccess` in `ProgramRepresentation.scala`.
        *
        * The code generated to implement a variable access may modify the value
        * of Reg.scratch. If the access is a read, it may also of course modify
        * the target register. If you need more than these registers, you may
        * add new scratch registers to Reg.scala. The generated code must not
        * modify the values of any other registers that are already listed in
        * Reg.scala.
        *
        * An error that students often make when implementing this method is to
        * assume that the staticLink and paramPtr are at constant offsets from
        * the beginning of a chunk. The offsets change depending on how many
        * variables and parameters a given function has. Do not assume that the
        * offsets are constant. Instead, use `Chunk.read` and `Chunk.write` with
        * the appropriate `Variable`s to access these values in the chunk.
        */
      def eliminateVarAccesses(code: Code): Code = {
        def eliminateHelper(current: Procedure, va: VarAccess): Code = {

          val currentFrame = phaseOneResults(current)._2

          if (paramChunks(current).variables.contains(va.variable)) {
            if (va.read) {
              block(
                currentFrame.load(Reg.scratch, Reg.scratch, current.paramPtr),
                paramChunks(current).load(Reg.scratch, va.register, va.variable)
              )
            } else {
              block(
                currentFrame.load(Reg.scratch, Reg.scratch, current.paramPtr),
                paramChunks(current)
                  .store(Reg.scratch, va.variable, va.register)
              )
            }
          } else if (currentFrame.variables.contains(va.variable)) {
            if (va.read)
              block(
                currentFrame.load(Reg.scratch, va.register, va.variable)
              )
            else
              block(
                currentFrame.store(Reg.scratch, va.variable, va.register)
              )
          } else {
            block(
              currentFrame.load(Reg.scratch, Reg.scratch, current.paramPtr),
              paramChunks(current)
                .load(Reg.scratch, Reg.scratch, current.staticLink),
              eliminateHelper(
                current.outer.get,
                va
              )
            )
          }
        }

        def fun: PartialFunction[Code, Code] = {
          case va: VarAccess => {
            block(
              move(Reg.scratch, Reg.framePointer),
              eliminateHelper(currentProcedure, va)
            )
          }
        }

        transformCode(code, fun)
      }

      val code1 = eliminateVarAccesses(code)
      val code2 = addEntryExit(code1)
      code2
    }

    /* Main body of compilerA6. */

    val code = block(
      heap.initCode,
      Stack.allocate(
        Chunk(procedures.head.parameters :+ procedures.head.staticLink)
      ), // allocate parameter chunk for start procedure
      Block(procedures.map(phaseTwo))
    )
    toMachineCode(code)
  }

}
