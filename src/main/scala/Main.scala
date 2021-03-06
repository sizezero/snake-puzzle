
/**
 * This application solves a wooden box puzzle:
 *
 * https://mypuzzles.wordpress.com/solution-the-snake-cube/
 */
object Main extends App {
  import org.kleemann.snakepuzzle.{Block,Solution,solve,prune,Snake}
  import org.kleemann.snakepuzzle.Block.{Straight,RightAngle}

  /**
   * The structure of an unpositioned 3x3x3 snake puzzle
   */
  val snake3x3x3 = List[Block](
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

  /**
   * The structure of an unpositioned 4x4x4 snake puzzle (Anaconda)
   */
  val snake4x4x4 = List[Block](
      Straight,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
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
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      Straight,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      Straight,
      RightAngle,
      RightAngle,
      Straight,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      RightAngle,
      Straight,
      Straight,
      RightAngle,
      Straight)

  def printRun(msg: String, snake: Snake) {

    def printSolutions(ss: List[Solution]) {
      // the functional way to iterate with an index
      ss.toStream.zipWithIndex.foreach{ case (s, zeroBased) =>
        val oneBased = zeroBased + 1
        println("solution #"+oneBased)
        println(s.toString)
        println
      }
    }

    val banner = "=" * 20
    println(banner + msg + banner)
    println
    val ss = solve(snake)
    println("All Solutions\n")
    printSolutions(ss)

    println("Pruned Solutions\n")
    printSolutions(prune(ss))
  }

  def timeTrial(snake: Snake) {
    import java.util.Calendar
    import java.util.concurrent.TimeUnit

    (0 to 64).foreach { i =>
      val start = Calendar.getInstance.getTimeInMillis()
      solve(snake, i) // throw away the result; we're just timing
      val end = Calendar.getInstance.getTimeInMillis()
      val duration = end - start
      val hours: Double = duration / (1000.0 * 60)
      println("%02d %f".format(i, hours))
    }
  }

  // error handling for turning a list of blocks into a snake
  def blocksToSnake(bs: List[Block], fn: Snake => Unit) {
    Snake(bs) match {
      case Left(msg) => println(msg)
      case Right(snake) => fn(snake)
    }
  }

  blocksToSnake(snake3x3x3, { printRun("3x3x3", _) } )

  // 4x4x4 puzzle takes around 3 minutes
  blocksToSnake(snake4x4x4, { printRun("4x4x4", _) } )
}
