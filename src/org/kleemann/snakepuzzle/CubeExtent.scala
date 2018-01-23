
package org.kleemann.snakepuzzle {

  /**
   * Keeps track of the size a cube is getting as blocks are placed in it.
   *
   * Each axis value in min is guaranteed to be less than or equal to
   * the corresponding axis value in max.
   */
  case class CubeExtent private (min: Coordinate, max: Coordinate) {

    /**
     * A successfully solved puzzle must have all pieces fit
     * within a cube of this size. I.e. all 27 blocks must
     * fit in a 3x3 cube.
     */
    val MAX_EXTENT = 3

    def add(c: Coordinate): CubeExtent =
      CubeExtent(
        Coordinate(min.x.min(c.x), min.y.min(c.y), min.z.min(c.z)),
        Coordinate(max.x.max(c.x), max.y.max(c.y), max.z.max(c.z)))

    def isLegal: Boolean =
      (min.x - max.x).abs < MAX_EXTENT &&
      (min.y - max.y).abs < MAX_EXTENT &&
      (min.z - max.z).abs < MAX_EXTENT
  }

  object CubeExtent {
    def firstPlacement(c: Coordinate) = CubeExtent(c, c)
  }

}