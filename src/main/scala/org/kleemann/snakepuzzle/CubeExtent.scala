
package org.kleemann.snakepuzzle {

  /** Keeps track of the size the bounding cube is getting as new block placements are made.
    *
    * A successfully solved puzzle must have all
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
    *
    *  - min.x <= max.x
    *  - min.y <= max.y
    *  - min.z <= max.z
    *
    * @param maxExtent the maximum extent of one edge of the cube (usually 3 or 4)
    * @param min the corner of the current extent with the smallest coordinates
    * @param max the corner of the current extent with the largest coordinates
    */
  case class CubeExtent private (maxExtent: Int, min: Coordinate, max: Coordinate) {

    /** Possibly increase this extent by adding a coordinate
      *
      * @return a new CubeExtent if the added Coordinate does not exceed the extent, None otherwise.
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
    /** Create a minimal CubeExtent
      *
      * @param maxExtent the maximum extent of one edge of the cube (usually 3 or 4)
      * @param c the singular coordinate of this extent: both max and min
      * @return the smallest CubeExtent of a single coordinate.
      *
      * @todo this should probably return an Either in case maxExtent is less than 1
      */
    def firstPlacement(maxExtent: Int, c: Coordinate) = CubeExtent(maxExtent, c, c)
  }

}