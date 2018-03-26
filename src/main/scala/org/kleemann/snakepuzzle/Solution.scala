
package org.kleemann.snakepuzzle {

  /** a complete and valid solution to the snake puzzle
    *
    * @param pbs placed blocks in the order they were placed
    */
  case class Solution private (pbs: List[PlacedBlock]) {
    override def toString: String = pbs.mkString("\n")
  }

  object Solution {

    /** Create a solution from a complete PartialSolution
      *
      * This is the only way to create a Solution
      *
      * @param p the PartialSolution to test for completeness
      * @return a Solution if the given PartialSolution is complete and valid
      */
    def isComplete(p: PartialSolution): Option[Solution] =
      if (p.isComplete) Some(Solution(p.pbs.reverse))
      else None
  }
}