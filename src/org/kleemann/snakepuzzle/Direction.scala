
package org.kleemann.snakepuzzle {

  /**
   *  One of the 6 right angle directions that can be traveled in 3d space
   */
  sealed trait Direction {
    /**
     * Given the starting Coordinate move one space in the given direction
     * and return the destination Coordinate.
     */
    def move(c: Coordinate): Coordinate

    /**
     * Returns the four perpendicular right angles to the given direction
     */
    def rightAngle: List[Direction]
  }

  object Direction {
    private def xAxisRightAngle = List[Direction](Up,Down,In,Out)

    object Right extends Direction {
      override val toString = "Right"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x+1, c.y, c.z)
      override def rightAngle = xAxisRightAngle
    }

    object Left extends Direction {
      override val toString = "Left"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x-1, c.y, c.z)
      override def rightAngle = xAxisRightAngle
    }

    private val yAxisRightAngle = List[Direction](Left,Right,In,Out)

    object Up extends Direction {
      override val toString = "Up"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y+1, c.z)
      override def rightAngle = yAxisRightAngle
    }

    object Down extends Direction {
      override val toString = "Down"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y-1, c.z)
      override def rightAngle = yAxisRightAngle
    }

    private val zAxisRightAngle = List[Direction](Left,Right,Up,Down)

    object In extends Direction {
      override val toString = "In"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y, c.z+1)
      override def rightAngle = zAxisRightAngle
    }

    object Out extends Direction {
      override val toString = "Out"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y, c.z-1)
      override def rightAngle = zAxisRightAngle
    }
  }

}
