import AtariGo.Stone.Stone

import scala.util.Random

object AtariGo {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int) //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  trait Random { //interface Random
    def nextInt(x: Int): (Int, Random)
  }

  case class MyRandom(seed: Long) extends Random {
    def nextInt(x: Int): (Int, Random) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = ((newSeed >>> 16).toInt) % x
      (if (n < 0) -n else n, nextRandom)
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: Random): (Coord2D, Random) = {
    val (randInt, newRand) = rand.nextInt(lstOpenCoords.length)
    (lstOpenCoords(randInt), newRand)
  }

  def main(args : Array[String]): Unit = {
    val k = List((0,0), (0,1), (1,1))
    val i = MyRandom(System.currentTimeMillis())
    val (move, newRand) = randomMove(k, i)
    println(s"Random move: $move")
  }
}