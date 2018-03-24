
package org.kleemann.snakepuzzle {

  /** One of the 6 right angle directions that can be traveled in 3d space
    */
  sealed trait Direction {
    /** @return the Coordinate resulting from moving the givn Coordinate in this Direction
      *
      * @param c the starting Coordinate of the move
      */
    def move(c: Coordinate): Coordinate

    /** @return the four perpendicular right angles to the given direction
      */
    def rightAngle: List[Direction]
  }

  object Direction {
    private val xAxisRightAngle = List[Direction](Up,Down,In,Out)
    private val yAxisRightAngle = List[Direction](Left,Right,In,Out)
    private val zAxisRightAngle = List[Direction](Left,Right,Up,Down)

    object Right extends Direction {
      override val toString = "Right"
      override def move(c: Coordinate) = Coordinate(c.x+1, c.y, c.z)
      override def rightAngle = xAxisRightAngle
    }

    object Left extends Direction {
      override val toString = "Left"
      override def move(c: Coordinate) = Coordinate(c.x-1, c.y, c.z)
      override def rightAngle = xAxisRightAngle
    }

    object Up extends Direction {
      override val toString = "Up"
      override def move(c: Coordinate) = Coordinate(c.x, c.y+1, c.z)
      override def rightAngle = yAxisRightAngle
    }

    object Down extends Direction {
      override val toString = "Down"
      override def move(c: Coordinate) = Coordinate(c.x, c.y-1, c.z)
      override def rightAngle = yAxisRightAngle
    }

    object In extends Direction {
      override val toString = "In"
      override def move(c: Coordinate) = Coordinate(c.x, c.y, c.z+1)
      override def rightAngle = zAxisRightAngle
    }

    object Out extends Direction {
      override val toString = "Out"
      override def move(c: Coordinate) = Coordinate(c.x, c.y, c.z-1)
      override def rightAngle = zAxisRightAngle
    }

    /** an arbitrary first direction that all solutions will start with
      *
      * By explicitly defining this, we can make some optimizations later.
      */
    val first = In
  }

}
