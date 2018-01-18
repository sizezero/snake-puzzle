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

    recurse(snake.tail, Solution.first)
  }

  /**
   * Prune solutions that are the same shape but rotated
   */
  val prunedSolutions: List[Solution] = {

    val startingDirection = PlacedBlock.first.d

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
        // keep this solution but filter out all following elements
        // that are rotated versions of this solution
        val h :: t = z
        val (s, ds) = h

        // from the first element generate a set of all rotated solution variants
        val rot1 = ds  .map{ _.rotate(startingDirection) }
        val rot2 = rot1.map{ _.rotate(startingDirection) }
        val rot3 = rot2.map{ _.rotate(startingDirection) }
        val variants = Set(ds, rot1, rot2, rot3)

        recurse(t.filter{ case (s2, ds2) => !(variants contains ds2) }, s :: accum)
      }
    }
    // Return list is built in reverse order using an accumulator to allow tail recursion.
    // Reverse again so the order matches the original solution list.
    recurse(ss, Nil).reverse
  }

  /**
   * Prune solutions that are the same shape but rotated.
   * This is an alternative implementation that attempts to use standard collection methods.
   */
  val prunedSolutions2: List[Solution] = {

    val startingDirection = PlacedBlock.first.d

    // a more compact way of representing a solutions
    // no coordinates, no bounding space: easy to rotate and compare
    type Directions = List[Direction]

    type Rotations = Set[Directions]

    def allRotations(ds: Directions): Rotations = {
        val rot1 = ds  .map{ _.rotate(startingDirection) }
        val rot2 = rot1.map{ _.rotate(startingDirection) }
        val rot3 = rot2.map{ _.rotate(startingDirection) }
        Set(ds, rot1, rot2, rot3)
    }

    // We could do this all in one expression but breaking it up allows us to
    // more easily verify the types of the intermediate values.

    // create a list of solutions that are pairs of
    // 1) a solution
    // 2) a matching Set of all rotated solutions
    val ss: List[(Solution,Rotations)] =
      allSolutions.zip(allSolutions.map{ s => allRotations(s.pbs.map{ _.d }) })

    // group solutions that differ only by their rotation
    val m: Map[Rotations, List[(Solution,Rotations)]] =
      ss.groupBy{ case (s, dss) => dss }

    // return the first solution in each group;
    // they are all just rotations of each other so it doesn't matter which one we end up using
    m.values.map{ ls => ls.head._1 }.toList
  }
}
