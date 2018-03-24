
package org.kleemann.snakepuzzle {

  /**
   * A single block in the link's chain.
   * This is the structure of an unplaced block.
   */
  sealed trait Block

  object Block {
    object Straight extends Block {
      override val toString = "Straight  "
    }
    object RightAngle extends Block {
      override val toString = "RightAngle"
    }
  }

}