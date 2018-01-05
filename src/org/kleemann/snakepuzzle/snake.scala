package org.kleemann

/**
 * This package contains the tools used to solve a wooden box puzzle:
 *
 * https://mypuzzles.wordpress.com/solution-the-snake-cube/
 *
 * TODO: I'm trying to do this functionally and I'm not sure if this should be a package or a trait.
 */
package object snakepuzzle {

  import scala.annotation.tailrec

  import org.kleemann.snakepuzzle.Block._

  /**
   * The structure of an unpositioned snake puzzle
   */
  val snake: List[Block] = List(
      Straight,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      Straight)

  /**
   * A successfully solved puzzle must have all pieces fit
   * within a cube of this size. I.e. all 27 blocks must
   * fit in a 3x3 cube.
   */
  val MAX_EXTENT = 3

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
     * Given a new block type, return zero or more partial solutions
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
    def firstPlacement(pb: PlacedBlock): Solution = Solution(List(pb), CubeExtent.firstPlacement(pb.c), Set(pb.c))
  }

  // arbitrary first placement
  val FIRST_PLACEMENT = PlacedBlock(snake.head, Coordinate.origin, Direction.In)

  /**
   * All legal solutions including duplicate rotated and symmetrical solutions
   */
  val allSolutions: List[Solution] = {

    /**
     * The recursive depth first search of all arrangements of the snake.
     * Legal solutions are kept and returned.
     *
     * partialSolution is built backwards; the first move is the final element of the list
     */
    def recurse(
        remainingSnake: List[Block], // sublist of snake: the remaining blocks to try
        partialSolution: Solution, // current, in progress solution
          ): List[Solution] = {

      if (remainingSnake == Nil) List(partialSolution) // partial solution has completed successfully
      else {
        partialSolution.next(remainingSnake.head). // find all legal moves of placing the next block
          flatMap{ recurse(remainingSnake.tail, _) } // and recurse
      }
    }

    recurse(snake.tail, Solution.firstPlacement(FIRST_PLACEMENT))
  }

  /**
   * Prune solutions that are the same shape but rotated
   */
  val prunedSolutions: List[Solution] = {

    val startingDirection = FIRST_PLACEMENT.d

    // create a list of solutions that are pairs of
    // 1) a solution
    // 2) a matching list of directions
    //
    // Solutions are big, complicated objects so when we make rotations to compare
    // we use a simpler list of Directions instead.
    val ss: List[(Solution,List[Direction])] = allSolutions.zip(allSolutions.map{ _.pbs.map{ _.d } })

    @tailrec
    def recurse(z: List[(Solution,List[Direction])], accum: List[Solution]): List[Solution] = {
      if (z == Nil) accum
      else {
        val h :: t = z
        val (s, ds) = h

        // from the first element generate a set of all rotated solution variants
        val rot1 = ds  .map{ _.rotate(startingDirection) }
        val rot2 = rot1.map{ _.rotate(startingDirection) }
        val rot3 = rot2.map{ _.rotate(startingDirection) }
        val variants = Set(ds, rot1, rot2, rot3)

        // filter all following solutions that may contain the rotated solutions and recurse
        recurse(t.filter{ case (s2, ds2) => !(variants contains ds2) }, s :: accum)
      }
    }
    // Return list is built in reverse order using an accumulator to allow tail recursion.
    // Reverse again so the order matches the original solution list.
    recurse(ss, Nil).reverse
  }
}
