
package org.kleemann.snakepuzzle {

  /** A coordinate in 3D space.
    *
    * If the viewer is looking at a window on a wall then:
    *
    *  - positive x moves to the right
    *  - positive y moves up
    *  - positive z moves toward the viewer
    *
    * This is a [[https://www.evl.uic.edu/ralph/508S98/coordinates.html
    * right-handed coordinate system with a rotation]].
    *
    */
  case class Coordinate(x: Int, y: Int, z: Int) {
    override def toString: String = "(%2d,%2d,%2d)".format(x,y,z)
  }

  object Coordinate {
    /** The origin of the coordinate system
      */
    val origin = Coordinate(0, 0, 0)
  }

}