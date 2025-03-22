import scala.util.Random

object AtariGo {
  type Board = List[List[Stone.Stone]]

  type Coord2D = (Int, Int) //(row, column)

  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  class MyRandom {
    val random: Random = new Random
    
    def nextInt: (Int, MyRandom) = {
      val exInt: Int = random.nextInt()
      val exRand: MyRandom = new MyRandom
      (exInt, exRand)
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val exCoord: Coord2D = (0, 0)
    (exCoord, rand)
  }
}