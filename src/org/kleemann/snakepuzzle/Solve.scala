
package org.kleemann.snakepuzzle {

  object Solve {

    /**
     * All complete and valid solutions including duplicate rotated
     * and symmetrical solutions. If the length of the snake is not
     * a perfect cube then None is returned.
     */
    def solve(snake: List[Block]): Option[List[Solution]] = {

      // The recursive depth first search of all arrangements of the snake.
      // Legal solutions are kept and returned.

      def recurse(
          partialSolution: Solution, // current, in progress solution
            ): List[Solution] = {

        if (partialSolution.isComplete) List(partialSolution)
        else
          // find all legal ways of adding a another block to the current partialSolution
          // keep the resulting legal solutions and recurse
          partialSolution.nextLegalPlacements.
            flatMap{ recurse(_) }
      }

      Solution.first(snake).map { partialSolutionOfFirstPlacement =>
        recurse(partialSolutionOfFirstPlacement)
      }
    }
  }
}
