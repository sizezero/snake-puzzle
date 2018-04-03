
package org.kleemann.snakepuzzle {

  /** the structure of a snake before any block is placed
    *
    * @param bs the list of blocks that comprise the snake
    */
  case class Snake private(bs: List[Block], root: Int)

  object Snake {

    def apply(bs: List[Block]): Either[String,Snake] =
      intCubeRoot(bs.length) match {
        case Some(root) => Right(new Snake(bs, root))
        case None => Left("length of snake is not a perfect cube: "+bs.length)
      }

    /** returns an integer cube root
      *
      * Only checks for the first dozen integer roots. Larger
      * puzzles than this don't exist.
      *
      * @param perfectCube an integeer that we expect to be a perfect cube
      * @returns the cube root if the argument is a perfect cube
      *
      */
    private def intCubeRoot(perfectCube: Int): Option[Int] =
      (1 to 12).find{ root => root*root*root == perfectCube }
  }
}