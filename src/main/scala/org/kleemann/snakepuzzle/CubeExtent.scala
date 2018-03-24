
package org.kleemann.snakepuzzle {

  /**
   * Keeps track of the size the bounding cube is getting as new block
   * placements are made.  A successfully solved puzzle must have all
   * placed cubes fit within a bounding cube. E.g. a snake of length 27
   * must fit in a 3x3 bounding cube. If the bounding cube of placed
   * cubes is ever greater than the extent then the collective
   * placements are not (and can never be) a valid solution.
   *
   * Due to the private constructor, a CubeExtent object can never exceed
   * the given extent.
   *
   * Each axis value in min is guaranteed to be less than or equal to
   * the corresponding axis value in max.
   * min.x <= max.x
   * min.y <= max.y
   * min.z <= max.z
   */
  case class CubeExtent private (maxExtent: Int, min: Coordinate, max: Coordinate) {

    /**
     * Add the given coordination to the extent, expanding the extent if necessary.
     * Returns None if the given coordinated causes the CubeExtent to exceed the
     * maxExtent.
     */
    def add(c: Coordinate): Option[CubeExtent] = {
      val min2 = Coordinate(min.x.min(c.x), min.y.min(c.y), min.z.min(c.z))
      val max2 = Coordinate(max.x.max(c.x), max.y.max(c.y), max.z.max(c.z))
      if (
        max2.x - min2.x < maxExtent &&
        max2.y - min2.y < maxExtent &&
        max2.z - min2.z < maxExtent) Some(CubeExtent(maxExtent, min2, max2))
      else None
    }
  }

  object CubeExtent {
    def firstPlacement(maxExtent: Int, c: Coordinate) = CubeExtent(maxExtent, c, c)
  }

}