object AtariGo {

  type Board = List[List[Stone.Stone]]

  type Coord2D = (Int, Int) //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  trait Random {
    def nextInt: (Int, Random)
  }

  case class MyRandom(seed: Long) extends Random {
    def nextInt: (Int, Random) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) &
        0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRandom)
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (randInt, newRand) = rand.nextInt
    val coord = lstOpenCoords(randInt%lstOpenCoords.length) 
    (coord, rand)
  }
}