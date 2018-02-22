
package org.kleemann.snakepuzzle {

  /**
   * This is a Solution is either in the process of being solved
   * or has already been completed.
   * Some blocks have been placed, some may have yet to be placed.
   * If every block has been placed then this solution is a solved
   * and valid solution.
   *
   * A Solution has a private constructor thus every Solution
   * instance is guaranteed to only contain legal block placements:
   * 1) Two blocks cannot occupy the same Coordinate
   * 2) All blocks must fit into a bounding cube of size 3
   */
  case class Solution private (
      blocksToPlace: List[Block], // the remaining blocks that haven't yet been placed
      pbs: List[PlacedBlock], // the blocks that have been placed from most recent to oldest
      extent: CubeExtent, // the cached extent of previously placed blocks
      occupiedCoordinates: Set[Coordinate]) { // cached coordinates of previous placed blocks

    /**
     * The solution is a complete and valid solution
     */
    def isComplete: Boolean = blocksToPlace == Nil

    /**
     * If there are more blocks to be placed then place the next block in
     * all possible orientations and return the resulting valid Solutions.
     * If the Solution has been completed then there are no remaining
     * blocks to be placed and no Solutions will be returned.
     */
    def next: List[Solution] =
      if (isComplete) Nil
      else
        pbs.head.next(blocksToPlace.head). // get all possible ways that the next block could be placed
          flatMap{ pb => testLegalMove(pb) } // only keep the legal placements

    /**
     * Attempts to add PlacedBlock to the Solution. If it makes a legal move
     * a new partial Solution is returned.
     */
    private def testLegalMove(pb: PlacedBlock): Option[Solution] = {
      // first test if we have already filled that coordinate
      if (occupiedCoordinates contains pb.c) None
      else {
        // then see if it fits in the legal cube size
        val newExtent = extent.add(pb.c)
        if (newExtent.isLegal) Some(Solution(blocksToPlace.tail, pb :: pbs, newExtent, occupiedCoordinates + pb.c))
        else None
      }
    }

    override def toString: String = pbs.reverse.mkString("\n")
  }

  object Solution {

    /**
     * Produces a partial solution of the first move of the given snake.
     * It's not possible to make an illegal choice in the first move.
     * If the length of snake is not a perfect cube then None is returned.
     */
    def first(snake: List[Block]): Option[Solution] = {
      intCubeRoot(snake.length).map { root =>
        val pb = PlacedBlock.first(snake.head)
        // The cube root of the snake length is equal to the length of
        // each side of the resulting cube. This is the maximum
        // extent of the bounding box of a legal solution (the snake
        // must be arranged into a cube of these dimensions)
        val maxExtent = root
        Solution(
          snake.tail,
          List(pb),
          CubeExtent.firstPlacement(maxExtent, pb.c),
          Set(pb.c))
      }
    }

    /**
     * Returns the cube root if the argument is a perfect cube.
     * Only checks for the first dozen integer roots. Larger
     * puzzles than this don't exist.
     */
    private def intCubeRoot(perfectCube: Int): Option[Int] = {
      (1 to 12).find{ root => root*root*root == perfectCube }
    }
  }

}