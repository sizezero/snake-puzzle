
package org.kleemann.snakepuzzle {

  object Solve {

    import scala.collection.parallel.immutable.ParSeq

    /**
     * All complete and valid solutions including duplicate rotated
     * and symmetrical solutions. If snake is not legal then an error
     * message in Left is returned.
     *
     * parallelDepth instructs the algorithm to perform the depth first search
     * in parallel until this depth. After this depth, perform it in a
     * single thread.
     */
    def solve(snake: List[Block], parallelDepth: Int): Either[String,List[Solution]] = {

      // Given a partial solution, perform a recursive depth first search
      // of all remaining arrangements of the snake. Legal solutions are
      // kept and returned.
      def recurse(partialSolution: PartialSolution): List[Solution] =
        Solution.isComplete(partialSolution) match {
          case Some(s) => List(s)
          case None =>
            // find all legal ways of adding a another block to the current partialSolution
            // recurse and keep the resulting solutions
            partialSolution.nextLegalPlacements.
              flatMap{ recurse(_) }
        }

      // a parallel version of the recursive solution. Tests show that we should only
      // use the parallel version at the top of the search tree otherwise the overhead
      // will actually increase the total time
      def recurseParallel(partialSolution: PartialSolution, depth: Int): ParSeq[Solution] =
        Solution.isComplete(partialSolution) match {
          case Some(s) => ParSeq(s)
          case None =>
            if (depth < parallelDepth)
              partialSolution.nextLegalPlacements.
                par.flatMap{ recurseParallel(_, depth+1) }
            else
              partialSolution.nextLegalPlacements.
                flatMap{ recurse(_) }.par
        }

      // Create the starting partial solution with a single placement...
      PartialSolution.first(snake).map { partialSolutionOfFirstPlacement =>
        // ...if it is legal (which means the snake is legal) recurse
        // into the full depth first search
        if (parallelDepth <= 0)
          // don't waste any effort on parallelization
          recurse(partialSolutionOfFirstPlacement)
        else
          recurseParallel(partialSolutionOfFirstPlacement, 0).toList
      }
    }

    /**
     * Same as solve(snake,maxDepth) but use a heuristic for the
     * best guess of parallelDepth.
     */
    def solve(snake: List[Block]): Either[String,List[Solution]] = {
      /*
       *  On my machine I get these times
       * Single threaded:
       *   3x3x3: 100ms
       *   4x4x4: 10 minutes
       * multithreaded is more complex and depends on max depth.
       * The full timings for all values of maxdepth on the 4x4x4 cube
       * can be found in doc/timings-4x4x4.csv
       * The summary is that for very small max depth the results are
       * similar to single threaded. There is a broad minimum around 3 minutes
       * with a max depth of 6 through 26 after which it slowly rises up to
       * 15 minutes (worse than the single threaded solution). I'm surprised
       * the overhead slows it down that much but it clearly indicates that
       * it's not worth it to use parallel processing to solve the last bit
       * of the puzzle.
       *
       * Given this limited data, I think the best choice is to choose the maximum
       * remaining snake length to solve serially.  This will be at maxDepth 24 for
       * 4x4x4 which gives us a value of 40.
       */
      solve(snake, snake.length - 40)
    }

  }
}
