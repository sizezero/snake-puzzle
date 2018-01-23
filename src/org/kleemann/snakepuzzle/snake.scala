package org.kleemann

/**
 * This package contains the tools used to solve a wooden box puzzle:
 *
 * https://mypuzzles.wordpress.com/solution-the-snake-cube/
 *
 * TODO: I'm trying to do this functionally and I'm not sure if this should be a package or a trait.
 */
package object snakepuzzle {

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

  val first = Solution.first(snake.head)

  /**
   * All legal solutions including duplicate rotated and symmetrical solutions
   */
  val allSolutions: List[Solution] = {

    // The recursive depth first search of all arrangements of the snake.
    // Legal solutions are kept and returned.

    // partialSolution is built backwards; the first move is the final element of the list

    def recurse(
        remainingSnake: List[Block], // sublist of snake: the remaining blocks to try
        partialSolution: Solution, // current, in progress solution
          ): List[Solution] = {

      // If there are no remainingSnake blocks then all blocks have been
      // placed into the partialSolution. That means the partialSolution is
      // a complete and legal solution.
      if (remainingSnake == Nil) List(partialSolution)
      else {
        // find all legal ways of adding a another block to the current partialSolution
        // keep the resulting legal solutions and recurse
        partialSolution.next(remainingSnake.head).
          flatMap{ recurse(remainingSnake.tail, _) }
      }
    }

    recurse(snake.tail, first)
  }

  /**
   * Prune solutions that are the same shape but rotated.
   */
  val prunedSolutions: List[Solution] = {

    // All rotations will be around the axis of the starting direction.
    // Any duplicate rotated solutions should be around this axis.
    val startingDirection = first.pbs.head.d

    // A more compact way of representing a solution for this
    // function's purposes.
    // No coordinates, no bounding space: easy to rotate and compare.
    type Directions = List[Direction]

    // A Directions rotated 90 degrees around the startingDirection axis
    // in all four possible ways. These are the duplicate solutions we are
    // trying to get rid of.
    type Rotations = Set[Directions]

    def directionsToRotations(ds: Directions): Rotations = {
        val rot1 = ds  .map{ _.rotate(startingDirection) }
        val rot2 = rot1.map{ _.rotate(startingDirection) }
        val rot3 = rot2.map{ _.rotate(startingDirection) }
        Set(ds, rot1, rot2, rot3)
    }

    // We could do this all in one expression but breaking it up allows us to
    // more easily verify the types of the intermediate values.

    // create a list of solutions that are pairs of
    // 1) a solution
    // 2) a matching Set of all rotations of the paired solution
    val ss: List[(Solution,Rotations)] =
      allSolutions.zip(allSolutions.map{ s => directionsToRotations(s.pbs.map{ _.d }) })

    // group solutions that differ only by their rotation
    val m: Map[Rotations, List[(Solution,Rotations)]] =
      ss.groupBy{ case (s, r) => r }

    // return the first solution in each group;
    // they are all just rotations of each other so it doesn't matter which one we end up using
    m.values.map{ ls => ls.head._1 }.toList
  }
}
