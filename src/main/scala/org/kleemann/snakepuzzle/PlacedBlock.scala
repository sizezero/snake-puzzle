
package org.kleemann.snakepuzzle {

  import Block.{Straight,RightAngle}

  /**
   * A PlacedBlock represents a chosen placement of a block in the puzzle snake.
   * It is a combination of a Block type, a Coordinate in space where the block
   * was placed and the Direction from the previously placed block to this block.
   */
  case class PlacedBlock(b: Block, c: Coordinate, d: Direction) {
    /**
     * Given an existing PlacedBlock, find the coordinate and direction
     * of all possible placements of the following block.  These positions
     * may or may not be legal placements
     */
    def nextPlacements(newBlock: Block): List[PlacedBlock] = {
      // make a list of all directions coming from the previously placed block
      val ds: List[Direction] = b match {
        case Straight => List(d)
        case RightAngle => d.rightAngle
      }
      // map the Directions into newly placed blocks
      ds.map { d2 => PlacedBlock(newBlock, d2.move(c), d2) }
    }

    override def toString: String = "%10s %s => %s".format(b, c, d)
  }

  object PlacedBlock {

    /**
     * arbitrary first placement
     */
    def first(b: Block): PlacedBlock = PlacedBlock(b, Coordinate.origin, Direction.first)
  }

}