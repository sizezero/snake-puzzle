
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

      Solution.first(snake).map { partialSolutionOfFirstPlacement =>
        recurse(snake.tail, partialSolutionOfFirstPlacement)
      }
    }
  }
}
