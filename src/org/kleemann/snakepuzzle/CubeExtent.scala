
package org.kleemann.snakepuzzle {

  /**
   * Keeps track of the size the bounding cube is getting as new block
   * placements are made.  A successfully solved puzzle must have all
   * 27 placed cubes fit within a 3x3 bounding cube. If the bounding
   * cube of placed cubes is ever greater than 3x3 then the collective
   * placements are not (and can never be) a valid solution.
   *
   * Each axis value in min is guaranteed to be less than or equal to
   * the corresponding axis value in max.
   * min.x <= max.x
   * min.y <= max.y
   * min.z <= max.z
   */
  case class CubeExtent private (maxExtent: Int, min: Coordinate, max: Coordinate) {

    /**
     * Add the given coordination to the extent, expanding the extent if necessary
     *
     * I'm drinking the Scala punctuation kool-aide and using operators instead of
     * named functions.
     */
    def +(c: Coordinate): CubeExtent =
      if (c.x>=min.x && c.x<=max.x && c.y>=min.y && c.y<=max.y && c.z>=min.z && c.z<=max.z)
        // optimization: don't create a new object if the given Coordinate
        // is already within the current extent
        this
      else
        CubeExtent(
          maxExtent,
          Coordinate(min.x.min(c.x), min.y.min(c.y), min.z.min(c.z)),
          Coordinate(max.x.max(c.x), max.y.max(c.y), max.z.max(c.z)))

    def isLegal: Boolean =
      max.x - min.x < maxExtent &&
      max.y - min.y < maxExtent &&
      max.z - min.z < maxExtent
  }

  object CubeExtent {
    def firstPlacement(maxExtent: Int, c: Coordinate) = CubeExtent(maxExtent, c, c)
  }

}