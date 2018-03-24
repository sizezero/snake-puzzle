
package org.kleemann.snakepuzzle {

  /** A single unplaced block in the link's chain.
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