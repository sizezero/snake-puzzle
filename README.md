# snake-puzzle

My niece gave me the following wooden block puzzle for xmas. It's a connected snake of 27 blocks that is solved by twisting it so that it becaomes a 3x3 cube:

https://mypuzzles.wordpress.com/solution-the-snake-cube/

I played around with it a bit and solved it a couple times by accident. I then wondered if there were multiple correct solutions. Woo hoo! I found a problem that programming could solve!

This gave me a chance to exercise my rusty Scala skills and also try to write code in the most functional way possible.

The program emulates a chain of 27 wooden blocks. The structure of each block is represented by the [Block trait](../blob/master/src/org/kleemann/snakepuzzle/Block.scala).  There are two types of Blocks:
1. a Straight Block which connects the previous and next block in a linear fashion. There is no choice in positioning a straight block.
2. a Right Angle Block with forms a right angle with the previous block and can thus be positioned in four different ways.

The input is the ordered structure of the 27 blocks:

```scala
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
``` 

The program then performs a depth first search of all possible arrangements of the block list and keeps the legal ones (blocks cannot occupy the same space nor can any block exceed the extent of a 3x3 cube). After the final legal solutions are found, duplicate solutions that are just simple rotations of each other are removed. Two solutions result:

```
solution #1
Straight   ( 0, 0, 0) => In
Straight   ( 0, 0, 1) => In
RightAngle ( 0, 0, 2) => In
RightAngle (-1, 0, 2) => Left
RightAngle (-1, 0, 1) => Out
Straight   (-1, 1, 1) => Up
RightAngle (-1, 2, 1) => Up
RightAngle (-2, 2, 1) => Left
Straight   (-2, 1, 1) => Down
RightAngle (-2, 0, 1) => Down
RightAngle (-2, 0, 2) => In
RightAngle (-2, 1, 2) => Up
Straight   (-1, 1, 2) => Right
RightAngle ( 0, 1, 2) => Right
Straight   ( 0, 1, 1) => Out
RightAngle ( 0, 1, 0) => Out
RightAngle (-1, 1, 0) => Left
RightAngle (-1, 0, 0) => Down
RightAngle (-2, 0, 0) => Left
Straight   (-2, 1, 0) => Up
RightAngle (-2, 2, 0) => Up
Straight   (-1, 2, 0) => Right
RightAngle ( 0, 2, 0) => Right
Straight   ( 0, 2, 1) => In
RightAngle ( 0, 2, 2) => In
Straight   (-1, 2, 2) => Left
Straight   (-2, 2, 2) => Left

solution #2
Straight   ( 0, 0, 0) => In
Straight   ( 0, 0, 1) => In
RightAngle ( 0, 0, 2) => In
RightAngle (-1, 0, 2) => Left
RightAngle (-1, 0, 1) => Out
Straight   (-1,-1, 1) => Down
RightAngle (-1,-2, 1) => Down
RightAngle (-2,-2, 1) => Left
Straight   (-2,-1, 1) => Up
RightAngle (-2, 0, 1) => Up
RightAngle (-2, 0, 2) => In
RightAngle (-2,-1, 2) => Down
Straight   (-1,-1, 2) => Right
RightAngle ( 0,-1, 2) => Right
Straight   ( 0,-1, 1) => Out
RightAngle ( 0,-1, 0) => Out
RightAngle (-1,-1, 0) => Left
RightAngle (-1, 0, 0) => Up
RightAngle (-2, 0, 0) => Left
Straight   (-2,-1, 0) => Down
RightAngle (-2,-2, 0) => Down
Straight   (-1,-2, 0) => Right
RightAngle ( 0,-2, 0) => Right
Straight   ( 0,-2, 1) => In
RightAngle ( 0,-2, 2) => In
Straight   (-1,-2, 2) => Left
Straight   (-2,-2, 2) => Left
```
 
 The above two solutions might be symetrical versions of each other. I'll wait until I get the physical puzzle back to verify.