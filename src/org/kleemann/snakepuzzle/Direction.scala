
package org.kleemann.snakepuzzle {

    /**
   *  One of the 6 right angle directions that can be traveled in 3d space
   */
  trait Direction {
    /**
     * Given the starting Coordinate move one space in the given direction
     * and return the destination Coordinate.
     */
    def move(c: Coordinate): Coordinate

    /**
     * Returns the four perpendicular right angles to the given direction
     */
    def rightAngle: List[Direction]

    /**
     * Rotates this Direction along the given axis and return the rotated direction.
     * If the axis is parallel to the direction then no rotation is performed.
     * Right hand rotation: thumb=axis, index=this, middle=return
     */
    def rotate(axis: Direction): Direction
  }

  object Direction {
    private def xAxisRightAngle = List[Direction](Up,Down,In,Out)

    object Right extends Direction {
      override val toString = "Right"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x+1, c.y, c.z)
      override def rightAngle = xAxisRightAngle
      override def rotate(axis: Direction): Direction = axis match {
        case Right => this
        case Left  => this
        case Up    => Out
        case Down  => In
        case In    => Up
        case Out   => Down
      }
    }

    object Left extends Direction {
      override val toString = "Left"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x-1, c.y, c.z)
      override def rightAngle = xAxisRightAngle
      override def rotate(axis: Direction): Direction = axis match {
        case Right => this
        case Left  => this
        case Up    => In
        case Down  => Out
        case In    => Down
        case Out   => Up
      }
    }

    private val yAxisRightAngle = List[Direction](Left,Right,In,Out)

    object Up extends Direction {
      override val toString = "Up"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y+1, c.z)
      override def rightAngle = yAxisRightAngle
      override def rotate(axis: Direction): Direction = axis match {
        case Right => In
        case Left  => Out
        case Up    => this
        case Down  => this
        case In    => Left
        case Out   => Right
      }
    }

    object Down extends Direction {
      override val toString = "Down"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y-1, c.z)
      override def rightAngle = yAxisRightAngle
      override def rotate(axis: Direction): Direction = axis match {
        case Right => Out
        case Left  => In
        case Up    => this
        case Down  => this
        case In    => Right
        case Out   => Left
      }
    }

    private val zAxisRightAngle = List[Direction](Left,Right,Up,Down)

    object In extends Direction {
      override val toString = "In"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y, c.z+1)
      override def rightAngle = zAxisRightAngle
      override def rotate(axis: Direction): Direction = axis match {
        case Right => Down
        case Left  => Up
        case Up    => Right
        case Down  => Left
        case In    => this
        case Out   => this
      }
    }

    object Out extends Direction {
      override val toString = "Out"
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y, c.z-1)
      override def rightAngle = zAxisRightAngle
      override def rotate(axis: Direction): Direction = axis match {
        case Right => Up
        case Left  => Down
        case Up    => Left
        case Down  => Right
        case In    => this
        case Out   => this
      }
    }
  }

}
