
package org.kleemann.snakepuzzle {

  /** A coordinate in 3D space.
    *
    * This is a [[https://www.evl.uic.edu/ralph/508S98/coordinates.html
    * right-handed coordinate system with a rotation]].
    *
    * If the viewer is looking at a window on a wall then:
    *
    * @param x positive x moves to the right
    * @param y positive y moves up
    * @param z positive z moves toward the viewer
    */
  case class Coordinate(x: Int, y: Int, z: Int) {
    override def toString: String = "(%2d,%2d,%2d)".format(x,y,z)
  }

  object Coordinate {
    /** The origin of the coordinate system
      */
    val origin = Coordinate(0, 0, 0)

    /** The arbitrary location of the first placed cube in every puzzle solution
      */
    val first = origin
  }

}