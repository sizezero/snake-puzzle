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

  /**
   * This is a partial solution made by placing the first block of the
   * snake in a more or less arbitrary placement. Every first move is
   * equally good since the only thing different is the orientation
   * (rotation) which doesn't matter.
   *
   * All partial solutions attempts and final, correct solutions are based
   * on this partial solution.
   */
  val first = Solution.first(snake.head)

  /**
   * All complete and valid solutions including duplicate rotated
   * and symmetrical solutions
   */
  val allSolutions: List[Solution] = {

    // The recursive depth first search of all arrangements of the snake.
    // Legal solutions are kept and returned.

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
   * Prune solutions that are the same shape but either rotated or mirrored.
   * These trivial variants should be considered the same solution.
   */
  val prunedSolutions: List[Solution] = {

    // All rotations will be around the axis of the starting direction.
    // Any duplicate rotated solutions should be around this axis.
    val startingDirection = first.pbs.head.d

    // A more compact way of representing a solution for this
    // function's purposes.
    // No coordinates, no bounding space: easy to rotate and compare.
    type Directions = List[Direction]

    // A Variant is a set of trivial ways in which a solution is not
    // different.
    // This can be a 90 degrees around the startingDirection axis
    // in all four possible ways. It can also be a mirror image.
    type Variant = Set[Directions]

    def removeVariants(ss: List[Solution], makeVariants: Directions => Variant): List[Solution] = {
      // create a list of solutions that are pairs of
      // 1) a solution
      // 2) a matching Set of all variants of the paired solution
      val ss2: List[(Solution,Variant)] =
        ss.zip(ss.map{ s => makeVariants(s.pbs.map{ _.d }) })

      // group solutions that differ only by their variant
      val m: Map[Variant, List[(Solution,Variant)]] =
        ss2.groupBy{ case (s, v) => v }

      // return the first solution in each group;
      // they are all just variants of each other so it doesn't matter which one we end up using
      m.values.map{ ls => ls.head._1 }.toList
    }

    def directionsToRotations(ds: Directions): Variant = {
        def rotate(ds2: Directions): Directions = ds2.map{ _.rotate(startingDirection) }
        val rot1 = rotate(ds)
        val rot2 = rotate(rot1)
        val rot3 = rotate(rot2)
        Set(ds, rot1, rot2, rot3)
    }

    def directionsToMirrors(ds: Directions): Variant = {
        val mir1: Directions = ds.map { d => d match {
          case Direction.Left => Direction.Right
          case Direction.Right => Direction.Left
          case _ => d
        }}
        val mir2: Directions = ds.map { d => d match {
          case Direction.Up => Direction.Down
          case Direction.Down => Direction.Up
          case _ => d
        }}
        val mir3: Directions = ds.map { d => d match {
          case Direction.Left => Direction.Right
          case Direction.Right => Direction.Left
          case Direction.Up => Direction.Down
          case Direction.Down => Direction.Up
          case _ => d
        }}
        Set(ds, mir1, mir2, mir3)
    }

    removeVariants(removeVariants(allSolutions, directionsToRotations), directionsToMirrors)
  }
}
