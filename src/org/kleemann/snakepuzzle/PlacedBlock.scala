
package org.kleemann.snakepuzzle {

  import Block._

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

    override def toString: String = "%10s %s => %s".format(b, c, d)
  }

  object PlacedBlock {

    /**
     * arbitrary first placement
     */
    def first(b: Block): PlacedBlock = PlacedBlock(b, Coordinate.origin, Direction.In)
  }

}