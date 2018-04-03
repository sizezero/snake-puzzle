package org.kleemann

package object snakepuzzle {

  // Scala doesn't allow package objects to be broken up into multiple files
  // so we must use these aliases if we want to put large function definitions
  // into separate files and have them accessible in the package scope

  /** Redirects to [[org.kleemann.snakepuzzle.Solve]]
    */
  def solve(snake: Snake, parallelDepth: Int) = Solve.solve(snake, parallelDepth: Int)

  /** Redirects to [[org.kleemann.snakepuzzle.Solve]]
    */
  def solve(snake: Snake) = Solve.solve(snake)

  /** Redirects to [[org.kleemann.snakepuzzle.Prune#prune]]
    */
  def prune(ss: List[Solution]) = Prune.prune(ss)

}
