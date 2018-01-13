
package org.kleemann.snakepuzzle {

  /**
   * The list of choices from most recent to oldest.
   *
   * A solution may be partial (not yet completed)
   *
   * A solution is guaranteed to be legal:
   * 1) Two blocks cannot occupy the same Coordinate
   * 2) All blocks must fit into a cube of size 3
   */
  case class Solution private (
      pbs: List[PlacedBlock], // A list of block placements from most recent to oldest
      extent: CubeExtent, // cache the extent of the shape of previously placed blocks
      occupiedCoordinates: Set[Coordinate]) { // cached coordinates of previous placed blocks

    /**
     * Given a new block type, return zero or more possibly partial but legal solutions
     */
    def next(b: Block): List[Solution] =
      pbs.head.next(b). // get all possible ways that the next block could be placed
        map{ testLegalMove(_) }.flatten // only keep the legal placements

    /**
     * Attempts to add PlacedBlock to the Solution and see if it makes a legal move
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
    val first = Solution(
        List(PlacedBlock.first),
        CubeExtent.firstPlacement(PlacedBlock.first.c),
        Set(PlacedBlock.first.c))
  }

}