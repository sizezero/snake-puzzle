package org.kleemann

/**
 * This package contains the tools used to solve a wooden box puzzle:
 *
 * https://mypuzzles.wordpress.com/solution-the-snake-cube/
 *
 * TODO: I'm trying to do this functionally and I'm not sure if this should be a package or a trait.
 */
package object snakepuzzle {

  /**
   * Represents a coordinate in 3D space.
   *
   * If the viewer is looking at a window on a wall then:
   * Positive x moves to the right
   * Positive y moves up
   * Positive z moves toward the viewer
   * This is a right-handed coordinate system with a rotation
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

  /**
   *  One of the 6 right angle direction that can be traveled in 3d space
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
     * Rotates this Direction along the given axes and return the rotated direction.
     * If the axis is parellel to the direction then no rotation is performed.
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
  import Direction._

  /**
   * A single block in the link's chain.
   */
  trait Block {
  }

  object Block {
    object Straight extends Block {
      override val toString = "Straight  "
    }
    object RightAngle extends Block {
      override val toString = "RightAngle"
    }
  }
  import Block._

  /**
   * The structure of an unpositioned snake puzzle
   */
  val snake: List[Block] = List(
      Straight,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      Straight)

  val MAX_EXTENT = 3

  /**
   * A PlacedBlock is a combination of a Block, a Coordinate in space and
   * the Direction of the next block in the chain.
   */
  case class PlacedBlock(b: Block, c: Coordinate, d: Direction) {
    /**
     * Given a block in space, find the coordinate and direction
     * of all possible placements of the following block.
     */
    def next(newBlock: Block): List[PlacedBlock] = b match {
      case Straight => List(PlacedBlock(newBlock, d.move(c), d))
      case RightAngle => d.rightAngle.map{ d2 => PlacedBlock(newBlock, d2.move(c), d2) }
    }

    override def toString: String = "%10s %s %s".format(b, c, d)
  }

  /**
   * Keeps track of the size a cube is getting as blocks are placed in it.
   */
  case class CubeExtent private (min: Coordinate, max: Coordinate) {

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

  /**
   * The list of choices from most recent to oldest.
   *
   * A solution may be partial (not yet completed)
   *
   * A solution is guaranteed to be legal:
   * 1) Two blocks cannot occupy the same Coordinate
   * 2) All blocks must fit into a cube of size 3
   */
  case class Solution private (

      /**
       * A list of block placements from most recent to oldest
       */
      pbs: List[PlacedBlock],

      /**
       * cache the extent of the shape of previously placed blocks
       */
      extent: CubeExtent,

      /**
       * cache the Coordinates of all previously placed blocks
       * for efficiency and ease
       */
      occupiedCoordinates: Set[Coordinate]) {

    /**
     * Given a new block type, return zero or more partial solutions
     */
    def next(b: Block): List[Solution] =
      pbs.head.next(b). // get all possible ways that the next block could be placed
        map{ testLegalMove(_) }. // test each of the new placements for legality
          flatten // Remove moves that were not legal

    /**
     * Attempts to add PlacedBlock to the Solution and see if it makes a legal move
     */
    private def testLegalMove(pb: PlacedBlock): Option[Solution] = {
      // first test if we have already filled that coordinate
      if (occupiedCoordinates contains pb.c) None
      else {
        // then see if it fits in a 3x3 cube
        val newExtent = extent.add(pb.c)
        if (newExtent.isLegal) Some(Solution(pb :: pbs, newExtent, occupiedCoordinates + pb.c))
        else None
      }
    }

    override def toString: String = pbs.reverse.mkString("\n")
  }

  object Solution {
    /**
     * It's not possible to make an illegal choice in the first move
     */
    def firstPlacement(pb: PlacedBlock): Solution = Solution(List(pb), CubeExtent.firstPlacement(pb.c), Set(pb.c))
  }

  // arbitrary first placement
  val FIRST_PLACEMENT = PlacedBlock(snake.head, Coordinate.origin, In)

  /**
   * All legal solutions including duplicate rotated and symmetrical solutions
   */
  val allSolutions: List[Solution] = {

    /**
     * The recursive depth first search of all arrangements of the snake.
     * Legal solutions are kept and returned.
     *
     * currentSolution is built backwards; the first move is the final element of the list
     */
    def recurse(
        /**
         * A sublist of snake. These are the remaining snake blocks that have yet to be used
         */
        remainingSnake: List[Block],

        /**
         * The current partial solution that we are working on
         */
        currentSolution: Solution,

        /**
         * This is the cumulative list of valid solutions
         */
        solutions: List[Solution]

          ): List[Solution] = {

      if (remainingSnake == Nil) currentSolution :: solutions
      else {
        currentSolution.next(remainingSnake.head). // find all legal moves of placing the next block
          flatMap{ recurse(remainingSnake.tail, _, solutions) } // and recurse
      }
    }

    recurse(snake.tail, Solution.firstPlacement(FIRST_PLACEMENT), Nil)
  }

  /**
   * Prune solutions that are the same shape but rotated
   */
  val prunedSolutions: List[Solution] = {

    val startingDirection = FIRST_PLACEMENT.d

    // create a list of solutions that are pairs of
    // 1) a solution
    // 2) a matching list of directions
    //
    // Solutions are big, complicated objects so when we make rotations to compare
    // we use a simpler list of Directions instead.
    val ss: List[(Solution,List[Direction])] = allSolutions.zip(allSolutions.map{ _.pbs.map{ _.d } })

    def recurse(z: List[(Solution,List[Direction])]): List[Solution] = {
      if (z == Nil) Nil
      else {
        val h :: t = z
        val (s, ds) = h

        // from the first element generate a set of all rotated solution variants
        val rot1 = ds  .map{ _.rotate(startingDirection) }
        val rot2 = rot1.map{ _.rotate(startingDirection) }
        val rot3 = rot2.map{ _.rotate(startingDirection) }
        val variants = Set(ds, rot1, rot2, rot3)

        // filter all following solutions that may contain the rotated solutions and recurse
        s :: recurse(t.filter{ case (s2, ds2) => !(variants contains ds2) })
      }
    }
    recurse(ss)
  }
}
