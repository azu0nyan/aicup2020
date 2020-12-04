package visualiser


object Event {
  type Listener[A] = A => Unit

  def apply[A]: Event[A] = new EventImpl[A]()
  //  def apply[A](): Event[A] = new EventImpl[A]()

  trait Event[Arg] {
    def subscribe(listener: Listener[Arg]): Listener[Arg]

    def unSubscribe(listener: Listener[Arg]): Unit

    def apply(arg: Arg): Unit = onEvent(arg)

    def onEvent(arg: Arg): Unit

    def onEventLazy(code: => Arg):Unit = onEvent(code)

  }


  class EventImpl[Arg] extends Event[Arg] {
    private var listeners: Vector[Listener[Arg]] = Vector()

    override def subscribe(listener: Listener[Arg]): Listener[Arg] = this.synchronized {
      listeners = listeners :+ listener
      return listener
    }

    override def unSubscribe(listener: Listener[Arg]): Unit = this.synchronized {
      listeners = listeners.filter(l => l != listener)
    }

    override def onEvent(arg: Arg): Unit = {
      listeners.foreach(_ (arg))
    }
    /**calculates arg only if listeners exists*/
    override def onEventLazy(code: => Arg): Unit = if(listeners.nonEmpty) onEvent(code)
  }

}




