
/**
 * This application solves a wooden box puzzle:
 *
 * https://mypuzzles.wordpress.com/solution-the-snake-cube/
 */
object Main extends App {
  import org.kleemann.snakepuzzle.{Block,Solution,solve,prune}
  import org.kleemann.snakepuzzle.Block.{Straight,RightAngle}

  /**
   * The structure of an unpositioned snake puzzle
   */
  val snake3x3x3: List[Block] = List(
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

  def printSolutions(ss: List[Solution]) {
    // the functional way to iterate with an index
    ss.toStream.zipWithIndex.foreach{ case (s, zeroBased) =>
      val oneBased = zeroBased + 1
      println("solution #"+oneBased)
      println(s.toString)
      println
    }
  }

  val allSolutions: Option[List[Solution]] = solve(snake3x3x3)

  if (allSolutions.isEmpty) {
    println("Error: length of snake is not a perfect cube")
  } else {
    val ss = allSolutions.orNull // can't be null

    println("All Solutions\n")
    printSolutions(ss)

    println("Pruned Solutions\n")
    printSolutions(prune(ss))
  }
}
