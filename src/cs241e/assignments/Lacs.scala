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

import Scanning.*
import Parsing.*
import Typer.*
import CodeGenerator.*
import Transformations.*
import MemoryManagement.*
import cs241e.nosource.ParsingPrivate
import cs241e.scanparse.*
import Grammars.*
import DFAs.*
import cs241e.assignments.ProgramRepresentation.Procedure
import scala.compiletime.ops.boolean

/** Implementations of the definitions from the Lacs language specification. */

object Lacs {

  /** The set of keywords defined in the Lacs language specification. */
  val keywords = Set("def", "var", "Int", "if", "else")

  /** The set of single-character tokens in the Lacs language specification
    * mapped to textual names.
    *
    * You may change the textual names if you wish, but you shouldn't need to.
    */
  val symbols = Map(
    ' ' -> "WHITESPACE",
    '\t' -> "WHITESPACE",
    '\n' -> "WHITESPACE",
    '\r' -> "WHITESPACE",
    '0' -> "ZERO",
    '<' -> "LT",
    '>' -> "GT",
    '=' -> "BECOMES",
    '+' -> "PLUS",
    '-' -> "MINUS",
    '*' -> "STAR",
    '/' -> "SLASH",
    '%' -> "PCT",
    '(' -> "LPAREN",
    ')' -> "RPAREN",
    '{' -> "LBRACE",
    '}' -> "RBRACE",
    ',' -> "COMMA",
    ';' -> "SEMI",
    ':' -> "COLON"
  )

  /** A DFA that recognizes any valid Lacs token from the list given in the Lacs
    * specification, (including COMMENT and WHITESPACE tokens). The alphabet
    * consists of every character that may appear in any token.
    *
    * A scanner needs to distinguish tokens for identifiers (with kind ID) from
    * tokens for various keywords (with kinds DEF, VAR, INT, etc.). One way to
    * achieve this is to recognize keywords in the DFA, so that the DFA ends in
    * a different state for each keyword. This leads to a large DFA, but is most
    * efficient. Another way is for the DFA to end in the same state for
    * identifiers and for all the keywords. In that case, the implementation of
    * the scanner needs to post-process the kinds of the resulting tokens to
    * correct the kinds of tokens with kind ID when the lexeme is one of the
    * keywords.
    *
    * You may take either of these two approaches. Specifically, the requirement
    * for this DFA is only that it recognize the set of valid Lacs token,
    * without necessarily ending in a distinct state for each kind of token.
    */

  val dfa = {

    DFA(
      alphabet =
        "<>=+-*/%(){},;:! \t\n\r".toSet ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'),
      states = Set(
        "start",
        "ID",
        "NUM",
        "ZERO",
        "LPAREN",
        "RPAREN",
        "LBRACE",
        "RBRACE",
        "BECOMES",
        "NOT",
        "EQ",
        "NE",
        "LT",
        "GT",
        "LE",
        "GE",
        "PLUS",
        "MINUS",
        "STAR",
        "SLASH",
        "PCT",
        "COMMA",
        "SEMI",
        "COLON",
        "ARROW",
        "COMMENT",
        "WHITESPACE"
      ),
      start = "start",
      accepting = Set(
        "ID",
        "NUM",
        "ZERO",
        "LPAREN",
        "RPAREN",
        "LBRACE",
        "RBRACE",
        "BECOMES",
        "EQ",
        "NE",
        "LT",
        "GT",
        "LE",
        "GE",
        "PLUS",
        "MINUS",
        "STAR",
        "SLASH",
        "PCT",
        "COMMA",
        "SEMI",
        "COLON",
        "ARROW",
        "COMMENT",
        "WHITESPACE"
      ),
      transition = {
        case ("start", c) if ('a' to 'z').contains(c.toLower) => "ID"
        case ("ID", c) if ('a' to 'z').contains(c.toLower)    => "ID"
        case ("ID", c) if ('0' to '9').contains(c)            => "ID"
        case ("start", '0')                                   => "ZERO"
        case ("start", c) if ('0' to '9').contains(c)         => "NUM"
        case ("NUM", c) if ('0' to '9').contains(c)           => "NUM"
        case ("start", '(')                                   => "LPAREN"
        case ("start", ')')                                   => "RPAREN"
        case ("start", '{')                                   => "LBRACE"
        case ("start", '}')                                   => "RBRACE"
        case ("start", '=')                                   => "BECOMES"
        case ("BECOMES", '=')                                 => "EQ"
        case ("BECOMES", '>')                                 => "ARROW"
        case ("start", '!')                                   => "NOT"
        case ("NOT", '=')                                     => "NE"
        case ("start", '<')                                   => "LT"
        case ("start", '>')                                   => "GT"
        case ("LT", '=')                                      => "LE"
        case ("GT", '=')                                      => "GE"
        case ("start", '+')                                   => "PLUS"
        case ("start", '-')                                   => "MINUS"
        case ("start", '*')                                   => "STAR"
        case ("start", '/')                                   => "SLASH"
        case ("start", '%')                                   => "PCT"
        case ("start", ',')                                   => "COMMA"
        case ("start", ';')                                   => "SEMI"
        case ("start", ':')                                   => "COLON"
        case ("SLASH", '/')                                   => "COMMENT"
        case ("COMMENT", c) if c != '\n'                      => "COMMENT"
        case ("start", c) if Set('\t', '\n', '\r', ' ').contains(c) =>
          "WHITESPACE"
      }
    )
  }

  /** A scanner for the Lacs programming language. Given an input string, scans
    * it into a sequence of tokens. The kinds of the tokens must correspond to
    * the kinds defined in the Lacs specification (e.g. ID, NUM, LPAREN, ...).
    * WHITESPACE and COMMENT tokens are removed from the sequence. The resulting
    * sequence is returned with a `Token("BOF")` before it and a `Token("EOF")`
    * after it. If the input string cannot be scanned by the maximal munch
    * algorithm, `sys.error()` is called with a suitable error message.
    *
    * Do not forget to enforce the following rule in the Lacs specification: A
    * Lacs program must not contain a pair of consecutive tokens that both come
    * from one of the following sets: {ID, DEF, VAR, INT, IF, ELSE, NUM} {EQ,
    * NE, LT, LE, GT, GE, BECOMES, ARROW}
    */
  def scan(input: String): Seq[Token] = {
    val excl1 = Set("ID", "DEF", "VAR", "INT", "IF", "ELSE", "NUM")
    val excl2 = Set("EQ", "NE", "LT", "LE", "GT", "GE", "BECOMES", "ARROW")

    val convertedTokens = maximalMunchScan(dfa, input)
      .map(token =>
        token match
          case Token("ID", lexeme) => {
            if (!keywords.contains(lexeme)) { token }
            else {
              lexeme match
                case "def"  => Token("DEF", lexeme)
                case "var"  => Token("VAR", lexeme)
                case "Int"  => Token("INT", lexeme)
                case "if"   => Token("IF", lexeme)
                case "else" => Token("ELSE", lexeme)
            }
          }
          case Token("ZERO", lexeme) => Token("NUM", lexeme)
          case _                     => token
      )

    def checkValidTokens(current: Token, tokens: Seq[Token]): Boolean = {
      if (tokens.isEmpty) {
        true
      } else {
        if (excl1.contains(current.kind) && excl1.contains(tokens.head.kind)) {
          false
        } else if (
          excl2.contains(current.kind) && excl2.contains(tokens.head.kind)
        ) {
          false
        } else {
          checkValidTokens(tokens.head, tokens.tail)
        }
      }
    }

    if (checkValidTokens(convertedTokens.head, convertedTokens.tail)) {
      convertedTokens.filter(token =>
        token.kind != "WHITESPACE" && token.kind != "COMMENT"
      )
    } else {
      sys.error("INVALID CONSECUTIVE TOKENS")
    }
  }

  /** The grammar for the Lacs programming language copied from the language
    * specification.
    */
  val grammar = parseGrammar("""
S BOF defdefs EOF
defdefs defdef defdefs
defdefs defdef
defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE
parmsopt parms
parmsopt
parms vardef COMMA parms
parms vardef
vardef ID COLON type
type INT
type LPAREN typesopt RPAREN ARROW type
typesopt types
typesopt
types type COMMA types
types type
vardefsopt VAR vardef SEMI vardefsopt
vardefsopt
defdefsopt defdefs
defdefsopt
expras expra SEMI expras
expras expra
expra ID BECOMES expr
expra expr
expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
expr term
expr expr PLUS term
expr expr MINUS term
term factor
term term STAR factor
term term SLASH factor
term term PCT factor
factor ID
factor NUM
factor LPAREN expr RPAREN
factor factor LPAREN argsopt RPAREN
test expr NE expr
test expr LT expr
test expr LE expr
test expr GE expr
test expr GT expr
test expr EQ expr
argsopt args
argsopt
args expr COMMA args
args expr
                             """)

  /** Scans and parses a Lacs program, returning the parse tree. */
  def scanAndParse(input: String): Tree = {
    val tokens = scan(input).toIndexedSeq
    val tree = parseCYK(grammar, tokens).getOrElse {
      val longestPrefixKinds = ParsingPrivate.longestPrefix(grammar, tokens)
      val longestPrefixLexemes =
        tokens.map(_.lexeme).take(longestPrefixKinds.length).mkString(" ")
      sys.error("Parsing error; longest prefix: " + longestPrefixLexemes)
    }
    tree
  }

  /** Scans, parses, and type-checks a Lacs program. Returns the
    * `ProcedureScope`s representing the procedures and a map giving the `Type`
    * of each `Tree` that has one.
    */
  def scanAndParseAndType(input: String): TypedProcedures = {
    val tree = scanAndParse(input)
    typeTree(tree)
  }

  /** Scans, parses, and type-checks a Lacs program, and generates procedures
    * from it. Returns the corresponding `Procedure` objects.
    */
  def scanAndParseAndTypeAndGenerate(input: String): Seq[Procedure] = {
    val typedProcedures = scanAndParseAndType(input)
    generateProcedures(typedProcedures)
  }

  /** Compiles a Lacs program to MIPS machine language. */
  def compile(input: String): MachineCode = {
    val procedures = scanAndParseAndTypeAndGenerate(input)
    compilerA6(procedures)
  }

  /** Compiles a Lacs program and the `GarbageCollector` to MIPS machine
    * language.
    */
  def compileWithGarbageCollector(input: String): MachineCode = {
    withGC {
      val procedures = scanAndParseAndTypeAndGenerate(input)
      compilerA6(procedures ++ GarbageCollector.procedures)
    }
  }
}
