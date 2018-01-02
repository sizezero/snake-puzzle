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
  case class Coordinate(x: Int, y: Int, z:Int) {
      override def toString: String = "(%2d,%2d,%2d)".format(x,y,z)
  }
  
  /** 
   *  One of the 6 right angle direction that can be traveled in 3d space 
   */
  trait Direction {
    def move(c: Coordinate): Coordinate
  }
  
  object Direction {
    object Left extends Direction {
      override def move(c: Coordinate): Coordinate = Coordinate(c.x-1, c.y, c.z)
      override def toString: String = "Left"
    }
    object Right extends Direction {
      override def move(c: Coordinate): Coordinate = Coordinate(c.x+1, c.y, c.z)
      override def toString: String = "Right"
    }
    object Up extends Direction {
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y+1, c.z)
      override def toString: String = "Up"
    }
    object Down extends Direction {
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y-1, c.z)
      override def toString: String = "Down"
    }
    object In extends Direction {
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y, c.z+1)
      override def toString: String = "In"
    }
    object Out extends Direction {
      override def move(c: Coordinate): Coordinate = Coordinate(c.x, c.y, c.z-1)      
      override def toString: String = "Out"
    }
  }
  import Direction._
  
  /**
   * A single block in the link's chain
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
   * A choice is a combination of a block a vector and a direction.
   * It's a single choice that can be made in a step of moving the snake.
   */
  case class Choice(b: Block, c: Coordinate, d: Direction) {
    /**
     * Given a block in space, find the coordinate and direction of the connecting block(s)
     */
    def next(newBlock: Block): List[Choice] =
      d match {
      case Left | Right => b match {
        case Straight => List(Choice(newBlock, d.move(c), d))
        case RightAngle => List(
          Choice(newBlock, Up.move(c), Up),
          Choice(newBlock, Down.move(c), Down),
          Choice(newBlock, In.move(c), In),
          Choice(newBlock, Out.move(c), Out))
      }
      case Up | Down => b match {
        case Straight => List(Choice(newBlock, d.move(c), d))
        case RightAngle => List(
          Choice(newBlock, Left.move(c), Left),
          Choice(newBlock, Right.move(c), Right),
          Choice(newBlock, In.move(c), In),
          Choice(newBlock, Out.move(c), Out))
      }
      case In | Out => b match {
        case Straight => List(Choice(newBlock, d.move(c), d))
        case RightAngle => List(
          Choice(newBlock, Left.move(c), Left),
          Choice(newBlock, Right.move(c), Right),
          Choice(newBlock, Up.move(c), Up),
          Choice(newBlock, Down.move(c), Down))
      }
    }

    override def toString: String = "%10s %s %s".format(b, c, d)
  }

    /**
   * The structure of the snake puzzle
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
   * The list of choices from most recent to oldest.
   * A solution may be partial (not yet completed)
   * A solution is guaranteed to be legal.
   */
  case class Solution private (
      
      /**
       * A list of choices from most recent to oldest
       */
      cs: List[Choice],
      
      /**
       * cache the previous coords for efficiency and ease
       */
      val occupiedCoordinates: Set[Coordinate]) {
    
    /**
     * Given a new block type, return zero or more partial solutions 
     */
    def next(b: Block): List[Solution] = {
      // TODO: a lot is going on here; maybe explain it
      cs.head.next(b).map{ testLegalMove(_) }.flatten
    }
      
    /**
     * Attempts to add a single block to the solution and see if it makes a legal move
     */
    private def testLegalMove(ch: Choice): Option[Solution] = {
      // first test if we have already filled that coordinate
      if (occupiedCoordinates contains ch.c) None
      else {
        // test extent
        // TODO: it's probably less efficient to scan the list of coordinates many times
        // we could probably alternatively carry the max and mins of all these in the Solution object
        val minX = occupiedCoordinates.foldLeft(ch.c.x)((n,ch2) => ch2.x.min(n))
        val maxX = occupiedCoordinates.foldLeft(ch.c.x)((n,ch2) => ch2.x.max(n))
        if ((minX - maxX).abs >= MAX_EXTENT) None
        else {
          val minY = occupiedCoordinates.foldLeft(ch.c.y)((n,ch2) => ch2.y.min(n))
          val maxY = occupiedCoordinates.foldLeft(ch.c.y)((n,ch2) => ch2.y.max(n))
          if ((minY - maxY).abs >= MAX_EXTENT) None
          else {
            val minZ = occupiedCoordinates.foldLeft(ch.c.z)((n,ch2) => ch2.z.min(n))
            val maxZ = occupiedCoordinates.foldLeft(ch.c.z)((n,ch2) => ch2.z.max(n))
            if ((minZ - maxZ).abs >= MAX_EXTENT) None
            else Option(Solution(ch :: cs, occupiedCoordinates + ch.c))
          }
        }
        
      }
    }

    override def toString: String = cs.reverse.mkString("\n")
  }

  object Solution {
    /**
  	 * It's not possible to make an illegal choice in the first move
     */
    def first(ch: Choice): Solution = Solution(List(ch), Set(ch.c))
  }
    


  /**
   * All solutions including duplicate rotated and symmetrical solutions
   */
  val allSolutions: List[Solution] = {

    /**
     * The recursive depth first search of all arrangements of the snake
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
         * If we've reached the end of remainingSnake and the problem is solved then the solution is placed here
         */
        solutions: List[Solution]

        /**
         * Returns all successes of this sub-branch
         */
          ): List[Solution] = {

      if (remainingSnake == Nil) currentSolution :: solutions
      else {
        val ss = currentSolution.next(remainingSnake.head)
        ss.flatMap{recurse(remainingSnake.tail, _, solutions)}
      }
    }

    // We need to choose an arbitrary initial coordinate and initial direction so we have to perform the first step.
    recurse(snake.tail, Solution.first(Choice(snake.head, Coordinate(0,0,0), In)), Nil)
  }
}
