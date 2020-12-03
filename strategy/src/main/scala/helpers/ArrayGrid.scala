package helpers


import model.Vec2Int

class ArrayGrid[@specialized(Int, Boolean, Double, Float) T](array:Array[T], override val resolution:Vec2Int) extends Grid [T]{



  /** do not call directly, use "valueAt" for safety check */
  override def valueAtUnsafe(pos: Vec2Int): T = array(toFlatIndex(pos)).asInstanceOf[T]

  def setValue(pos:Vec2Int, newValue:T):Unit = {
    if(validIndex(pos)){
      array(toFlatIndex(pos)) = newValue
    }
  }

  def setValue(x:Int, y:Int, newValue:T):Unit = {
    if(validIndex(x, y)){
      array(toFlatIndex(x, y)) = newValue
    }
  }


  def setValue(flat:Int, newValue:T):Unit = {
    array(flat) = newValue
  }

  @inline override def apply(flat:Int):T = array(flat).asInstanceOf[T]
  @inline def apply(x:Int, y:Int):T = array(toFlatIndex(x, y)).asInstanceOf[T]

  @inline override def valueAt(flat:Int):T = array(flat).asInstanceOf[T]


}

