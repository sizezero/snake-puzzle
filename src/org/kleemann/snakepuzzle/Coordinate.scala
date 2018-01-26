
package org.kleemann.snakepuzzle {

  /**
   * Represents a coordinate in 3D space.
   *
   * If the viewer is looking at a window on a wall then:
   * Positive x moves to the right.
   * Positive y moves up.
   * Positive z moves toward the viewer.
   *
   * This is a right-handed coordinate system with a rotation.
   * https://www.evl.uic.edu/ralph/508S98/coordinates.html
   */
  case class Coordinate(x: Int, y: Int, z: Int) {
    override def toString: String = "(%2d,%2d,%2d)".format(x,y,z)
  }

  object Coordinate {
    /**
     * The origin is used several times so use the same object for efficiency and clarity
     */
    val origin = Coordinate(0, 0, 0)
  }

}