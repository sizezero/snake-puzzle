
package org.kleemann.snakepuzzle {

  object Prune {

    /**
     * Prune solutions that are the same shape but either rotated or mirrored.
     * These trivial variants should be considered the same solution.
     */
    def prune(ss: List[Solution]): List[Solution] = {

      // A more compact way of representing a solution for this
      // function's purposes.
      // No coordinates, no bounding space: easy to transform and compare.
      type Directions = List[Direction]

      def solutionToDirections(s: Solution): Directions = s.pbs.map{ _.d }

      // A Variants is a set of Solutions (represented as Directions) that
      // are all trivial variations of each other.
      // This can be a 90 degrees around the startingDirection axis
      // in all four possible ways. It can also be a mirror image.
      // There is no single correct solution in a set of variants.
      // Any one is as good as the other. The rest can be discarded.
      type Variants = Set[Directions]

      // given a list of solutions and a function that can turn Directions (a Solution) into
      // Variants (set of trivially different solutions), return a list of Solutions
      // that prunes all but one of each variant
      def removeVariants(ss: List[Solution], directionsToVariants: Directions => Variants): List[Solution] = {
        // create a list of solutions that are pairs of
        // 1) a solution
        // 2) a matching Set of all variants of the paired solution
        val z: List[(Solution,Variants)] =
          ss.zip(ss.map{ s => directionsToVariants(solutionToDirections(s)) })

        // group solutions that belong to the same set of variants
        val m: Map[Variants, List[(Solution,Variants)]] =
          z.groupBy{ case (s, v) => v }

        // return the first solution in each variant group;
        // they are all just variants of each other so it doesn't matter which one we end up using
        m.values.map{ ls => ls.head._1 }.toList
      }

      // to save us some work, the following two functions assume our starting direction is
      // Direction.In so all symmetry is around that direction
      assert(Direction.first == Direction.In)

      import Direction.{Left,Right,Up,Down}

      def rotationVariants(ds: Directions): Variants = {
        // Simple right hand rotation around Direction.In
        def rotate(ds2: Directions): Directions = ds2.map{ d => d match {
          case Left  => Down
          case Down  => Right
          case Right => Up
          case Up    => Left
          case _     => d
        }}
        val rot1 = rotate(ds)
        val rot2 = rotate(rot1)
        val rot3 = rotate(rot2)
        Set(ds, rot1, rot2, rot3)
      }

      def mirrorVariants(ds: Directions): Variants = {
        def flipHorz(d: Direction): Direction = d match {
          case Left  => Right
          case Right => Left
          case _     => d
        }
        def flipVert(d: Direction): Direction = d match {
          case Up   => Down
          case Down => Up
          case _    => d
        }
        val mir1 = ds.map { flipHorz(_) }
        val mir2 = ds.map { flipVert(_) }
        val mir3 = ds.map { d => flipHorz(flipVert(d)) }
        Set(ds, mir1, mir2, mir3)
      }

      // produce mirror variants and then rotate the mirrors
      def rotateAndMirrorVariants(ds: Directions): Variants =
        mirrorVariants(ds).flatMap{ rotationVariants(_) }

      removeVariants(ss, rotateAndMirrorVariants)

      // Post Analysis:
      // The above solution is an interesting functional way to solve the problem:
      //
      // 1) The input objects (Solution) were not implemented in the correct way to
      // allow variants to be detected and removed. Instead of refactoring the original
      // classes to allow for multiple purposes, simply transform the objects into a
      // more useful shape.
      //
      // 2) My first solution did not use custom types and signatures looked like this:
      // val m: Map[Set[List[Direction]], List[(Solution,Set[List[Direction]])]]
      // instead of
      // val m: Map[Variants, List[(Solution,Variants)]]
      // types are great when you want to write functionally and don't want to create
      // objects that couple methods and data
      //
      // 3) removeVariants is a higher order function that allows the variant defining behavior
      // to be cleanly separated from the other operations. I'm not sure I would have used a higher
      // order function if Scala didn't have such a clean syntax for passing functions as parameters.
    }
  }
}
