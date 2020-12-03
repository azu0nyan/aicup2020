package helpers

import model.Vec2Int

import scala.reflect.ClassTag


/** Grid with values at integer points, use indices or values to iterate over */
trait Grid[T] {

  /** Grid size, indices will be from (0, 0) to resolution - (1, 1) */
  def resolution: Vec2Int
  @inline def apply(pos:Vec2Int):T = valueAt(pos)

  /** pos will be clamped to  [(0, 0), resolution - (1, 1)] */
  @inline def valueAt(pos: Vec2Int): T = valueAtUnsafe(clampResolutionIndices(pos))

  @inline def apply(flat:Int):T = valueAt(fromFlatIndex(flat))

  @inline def valueAt(flat:Int):T = valueAt(fromFlatIndex(flat))

  @inline def validIndex(x:Int, y:Int): Boolean =
    x >= 0 && y >= 0 &&  x < resolution.x && y < resolution.y
  @inline def validIndex(pos: Vec2Int): Boolean =  validIndex(pos.x, pos.y)

  /** do not call directly, use "valueAt" for safety check */
  def valueAtUnsafe(pos: Vec2Int): T

  /** clamp to  [(0, 0), resolution - (1, 1)] */
  final def clampResolutionIndices(divIndex: Vec2Int): Vec2Int = Vec2Int(math.min(resolution.x - 1, math.max(divIndex.x, 0)),
    math.min(resolution.y - 1, math.max(divIndex.y, 0)))

  /** for storing grid in 1D array */
  @inline def toFlatIndex(index: Vec2Int): Int = index.x * resolution.y + index.y
  @inline def toFlatIndex(x:Int, y:Int): Int = x * resolution.y + y

  @inline def fromFlatIndex(index: Int): Vec2Int = Vec2Int(index / resolution.y, index % resolution.y)

  @inline def valuesCount: Int = resolution.x * resolution.y

  def indices: Iterator[Vec2Int] = for (i <- 0 until resolution.x iterator; j <- 0 until resolution.y iterator) yield Vec2Int(i, j)
//  def flatIndices: Iterator[Int] = for (i <- 0 until resolution.area iterator) yield i

  def values: Iterator[T] = indices.iterator.map(i => valueAt(i))

  def neighboursXY(i: Vec2Int): Seq[Vec2Int] =
    Seq(
      Option.when(i.x > 0)(Vec2Int(i.x - 1, i.y)),
      Option.when(i.y > 0)(Vec2Int(i.x, i.y - 1)),
      Option.when(i.x < resolution.x - 1)(Vec2Int(i.x + 1, i.y)),
      Option.when(i.y < resolution.y - 1)(Vec2Int(i.x, i.y + 1)),
    ).flatten

  def neighboursDiag(i: Vec2Int): Seq[Vec2Int] =
    Seq(
      Option.when(i.x > 0 && i.y > 0)(Vec2Int(i.x - 1, i.y - 1)),
      Option.when(i.x > 0 && i.y < resolution.y - 1)(Vec2Int(i.x - 1, i.y + 1)),
      Option.when(i.x < resolution.x - 1 && i.y > 0)(Vec2Int(i.x + 1, i.y - 1)),
      Option.when(i.x < resolution.x - 1 && i.y < resolution.y - 1)(Vec2Int(i.x + 1, i.y + 1)),
      Option.when(i.x > 0)(Vec2Int(i.x - 1, i.y)),
      Option.when(i.y > 0)(Vec2Int(i.x, i.y - 1)),
      Option.when(i.x < resolution.x - 1)(Vec2Int(i.x + 1, i.y)),
      Option.when(i.y < resolution.y - 1)(Vec2Int(i.x, i.y + 1)),
    ).flatten

  def toArrayGrid(implicit  c:ClassTag[T]):ArrayGrid[T] = new ArrayGrid[T](values.toArray, resolution)
}

