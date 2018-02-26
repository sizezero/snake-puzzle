
package org.kleemann.snakepuzzle {

  object Solve {

    /**
     * All complete and valid solutions including duplicate rotated
     * and symmetrical solutions. If snake is not legal then an error
     * message in Left is returned.
     */
    def solve(snake: List[Block]): Either[String,List[Solution]] = {

      // Given a partial solution, perform a recursive depth first search
      // of all remaining arrangements of the snake. Legal solutions are
      // kept and returned.
      def recurse(partialSolution: Solution): List[Solution] =
        if (partialSolution.isComplete) List(partialSolution)
        else
          // find all legal ways of adding a another block to the current partialSolution
          // recurse and keep the resulting solutions
          partialSolution.nextLegalPlacements.
            flatMap{ recurse(_) }

      // Create the starting partial solution with a single placement...
      Solution.first(snake).map { partialSolutionOfFirstPlacement =>
        // ...if it is legal (which means the snake is legal) recurse
        // into the full depth first search
        recurse(partialSolutionOfFirstPlacement)
      }
    }
  }
}
