import model.{Vec2Float, Vec2Int}

trait VecOps {
  implicit class VecOps(val x: Vec2Int) {

    def toProd:(Int, Int) = (x.x, x.y)

    def +(ot: Vec2Int): Vec2Int = Vec2Int(x.x + ot.x, x.y + ot.y)

    def distanceTo(ot: Vec2Int): Int = math.abs(x.x - ot.x) + math.abs(x.y - ot.y)

    def toVec2Float:Vec2Float = Vec2Float(x.x, x.y)
  }

  implicit class VecFOps(val x: Vec2Float) {
    def toVec2Int:Vec2Int = Vec2Int(x.x.toInt, x.y.toInt)

    def toProd:(Float, Float) = (x.x, x.y)
    def +(ot: Vec2Float): Vec2Float = Vec2Float(x.x + ot.x, x.y + ot.y)

    def -(ot: Vec2Float): Vec2Float = Vec2Float(x.x - ot.x, x.y - ot.y)

    def *(ot: Float): Vec2Float = Vec2Float(x.x * ot, x.y * ot)
    def *(ot: Vec2Float): Vec2Float = Vec2Float(x.x * ot.x, x.y * ot.y)

    def **(ot: Vec2Float): Float = x.x * ot.x +  x.y * ot.y

    def length:Float = math.sqrt(x ** x) toFloat

    def normalize:Vec2Float = {
      val l = length
      if(l != 0) x * (1 / l)
      else x
    }
  }

}
