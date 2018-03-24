
package org.kleemann.snakepuzzle {

  /**
   * A solution that is complete and valid.
   *
   * pbs are the blocks placed in order.
   * The first block placed is first in the list.
   */
  case class Solution private (pbs: List[PlacedBlock]) {
    override def toString: String = pbs.mkString("\n")
  }

  object Solution {

    /**
     * The only way to create a Solution is from a complete PartialSolution
     */
    def isComplete(p: PartialSolution): Option[Solution] =
      if (p.isComplete) Some(Solution(p.pbs.reverse))
      else None
  }
}