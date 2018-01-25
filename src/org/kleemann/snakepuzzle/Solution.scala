
package org.kleemann.snakepuzzle {

  /**
   * The list of choices from most recent to oldest.
   *
   * A solution may be partial (not yet completed)
   *
   * A Solution has a private constructor thus every Solution
   * instance is guaranteed to only contain legal block placements:
   * 1) Two blocks cannot occupy the same Coordinate
   * 2) All blocks must fit into a bounding cube of size 3
   */
  case class Solution private (
      pbs: List[PlacedBlock], // A list of block placements from most recent to oldest
      extent: CubeExtent, // the cached extent of previously placed blocks
      occupiedCoordinates: Set[Coordinate]) { // cached coordinates of previous placed blocks

    /**
     * Given a new block type, place it onto the previously placed block
     * and return zero or more possibly partial but legal solutions.
     */
    def next(b: Block): List[Solution] =
      pbs.head.next(b). // get all possible ways that the next block could be placed
        flatMap{ testLegalMove(_) } // only keep the legal placements

    /**
     * Attempts to add PlacedBlock to the Solution and see if it makes a legal move
     * and thus produces a new partial Solution.
     */
    private def testLegalMove(pb: PlacedBlock): Option[Solution] = {
      // first test if we have already filled that coordinate
      if (occupiedCoordinates contains pb.c) None
      else {
        // then see if it fits in a 3x3 cube
        val newExtent = extent.add(pb.c)
        if (newExtent.isLegal) Some(Solution(pb :: pbs, newExtent, occupiedCoordinates + pb.c))
        else None
      }
    }

    override def toString: String = pbs.reverse.mkString("\n")
  }

  object Solution {

    /**
     * It's not possible to make an illegal choice in the first move
     */
    def first(b: Block) = {
      val pb = PlacedBlock.first(b)
      Solution(
        List(pb),
        CubeExtent.firstPlacement(pb.c),
        Set(pb.c))
    }
  }

}