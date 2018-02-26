package org.kleemann

package object snakepuzzle {

  // scala doesn't allow package objects to be broken up into multiple files
  // so we must use these aliases if we want to put large function definitions
  // into separate files and have them accessible in the package scope

  def solve(snake: List[Block]): Either[String,List[Solution]] = Solve.solve(snake)

  def prune(ss: List[Solution]): List[Solution] = Prune.prune(ss)

}
