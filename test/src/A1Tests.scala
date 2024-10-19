import cs241e.assignments.{Assembler, A1}
import org.scalatest.funsuite.AnyFunSuite

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A1Tests extends AnyFunSuite {
  test("decodeUnsigned") {

    assert(1 + 1 == 2, "1 + 1 did not equal 2.")

    val testAllOnes = Seq(true, true, true, true, true, true, true, true, true,
      true, true, true, true, true, true, true, true, true, true, true, true,
      true, true, true, true, true, true, true, true, true, true, true)

    // The following will fail until you implement decodeUnsigned as part of Assignment 1.
  }
}
