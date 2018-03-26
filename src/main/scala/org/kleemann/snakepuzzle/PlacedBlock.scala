
package org.kleemann.snakepuzzle {

  import Block.{Straight,RightAngle}

  /** a chosen placement of a block in the puzzle snake
    *
    * @param b the structure of the Block that was placed
    * @param c the Coordinate of the placed block in space
    * @param d the Direction from the previously placed block to this block
    */
  case class PlacedBlock(b: Block, c: Coordinate, d: Direction) {
    /** Given a new Block, return all possible ways it can be placed
      *
      * These positions are only based on the structure of this
      * block (Straight or RightAngle) and may or may not be legal placements.
      *
      * @param newBlock the structure of the next PlacedBlock
      * @return all possible placements when a new block is added to this block
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

    /** arbitrary first placement
      */
    def first(b: Block): PlacedBlock = PlacedBlock(b, Coordinate.origin, Direction.first)
  }

}