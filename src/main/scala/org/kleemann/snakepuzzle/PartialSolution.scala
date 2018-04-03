
package org.kleemann.snakepuzzle {

  /** a Solution that is either in the process of being solved or has already been completed
    *
    * Some blocks have been placed, some may have yet to be placed.
    * If every block has been placed then this solution is a solved
    * and valid solution.
    *
    * A Solution has a private constructor thus every Solution
    * instance is guaranteed to only contain legal block placements:
    *
    *  1. Two blocks cannot occupy the same Coordinate
    *  1. All blocks must fit into a bounding cube dependent on the length of the original snake
    *
    * @param bs the remaining blocks that haven't yet been placed; next block to place is first in list
    * @param pbs the blocks that have been placed; most recent block is first in list
    * @param extent the cached extent of previously placed blocks
    * @param occupiedCoordinates cached coordinates of previous placed blocks
    */
  case class PartialSolution private (
      private val bs: List[Block],
              val pbs: List[PlacedBlock], // docs for this are omitted from scaladoc
      private val extent: CubeExtent,
      private val occupiedCoordinates: Set[Coordinate]) {

    /** Returns true if the PartialSolution is a complete and valid solution
      */
    def isComplete: Boolean = bs == Nil

    /** Returns all legal PartialSolutions that result from placing the next block
      *
      * If there are more blocks to be placed then place the next block in
      * all possible orientations and return the resulting valid partial
      * Solutions.
      *
      * If the partial Solution has been completed then there are no remaining
      * blocks to be placed then no PartialSolutions will be returned.
      *
      * @return all PartialSolutions that result from placing the next block in all legal ways
      */
    def nextLegalPlacements: List[PartialSolution] =
      if (isComplete) Nil
      else
        pbs.head.nextPlacements(bs.head). // get all possible ways that the next block could be placed
          flatMap{ pb => testLegalMove(pb) } // only keep the legal placements

    /** @return a new PartialSolution if the placed block is a legal move
      *
      * @params bp a placed block which results in one of the placements of bs.head
      */
    private def testLegalMove(pb: PlacedBlock): Option[PartialSolution] =
      // first test if we have already filled that coordinate
      if (occupiedCoordinates contains pb.c) None
      else
        // then see if it fits in the legal cube size
        extent.add(pb.c).map { newExtent =>
          PartialSolution(bs.tail, pb :: pbs, newExtent, occupiedCoordinates + pb.c)
        }
  }

  object PartialSolution {

    /** given the structure of a snake, produce a Partial Solution by placing the first block
      *
      * It's not possible to make an illegal choice in the first move.
      *
      * If the length of snake is not a perfect cube then the left
      * part of Either contains the error message.
      *
      * @param snake a list of blocks to be placed
      * @return an error message if the length of the snake is not a perfect cube or a PartialSolution with one block placed
      */
    def first(snake: Snake): PartialSolution = {
          // The cube root of the snake length is equal to the length of
          // each side of the resulting cube. This is the maximum
          // extent of the bounding box of a legal solution (the snake
          // must be arranged into a cube of these dimensions)
          val maxExtent = snake.root
          val pb = PlacedBlock.first(snake.bs.head)
          PartialSolution(
            snake.bs.tail,
            List(pb),
            CubeExtent.firstPlacement(maxExtent, pb.c),
            Set(pb.c))
      }
    }
}