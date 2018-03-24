package org.kleemann

package object snakepuzzle {

  // Scala doesn't allow package objects to be broken up into multiple files
  // so we must use these aliases if we want to put large function definitions
  // into separate files and have them accessible in the package scope

  def solve(snake: List[Block], parallelDepth: Int) = Solve.solve(snake, parallelDepth: Int)

  def solve(snake: List[Block]) = Solve.solve(snake)

  def prune(ss: List[Solution]) = Prune.prune(ss)

}
