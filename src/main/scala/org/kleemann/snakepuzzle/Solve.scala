
package org.kleemann.snakepuzzle {

  object Solve {

    /** Returns all complete solutions of the given snake
      *
      * If snake is not legal then an error message in Left is returned.
      *
      * @param snake the structure of the snake to solve
      * @param parallelDepth instructs the algorithm to perform the depth first search in parallel until this depth. After this depth, perform it in a single thread.
      * @return all complete and valid solutions including duplicate rotated and symmetrical solutions
      */
    def solve(snake: Snake, parallelDepth: Int): List[Solution] = {

      // Given a partial solution, perform a recursive depth first search
      // of all remaining arrangements of the snake. Legal solutions are
      // kept and returned.
      def recurse(partialSolution: PartialSolution, depth: Int): List[Solution] =
        Solution.isComplete(partialSolution) match {
          case Some(s) => List(s)
          case None =>
            if (depth >= parallelDepth)
              // find all legal ways of adding a another block to the current partialSolution
              // recurse and keep the resulting solutions
              partialSolution.nextLegalPlacements.
                flatMap{ recurse(_, depth+1) }
            else
              // The is functionally identical to the above clause; it is just
              // parallelized.
              // It's only efficient to use parallelization at the top of the
              // search tree, not near the leaves.
              partialSolution.nextLegalPlacements.
                par.flatMap{ recurse(_, depth+1) }.toList
        }

      // Create the starting partial solution with a single placement
      // and recurse into the full depth first search
      recurse(PartialSolution.first(snake), 0)
    }

    /** Same as solve(snake,parallelDepth) but use a heuristic for the best guess of parallelDepth.
      */
    def solve(snake: Snake): List[Solution] = {
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
      solve(snake, snake.bs.length - 40)
    }

  }
}
