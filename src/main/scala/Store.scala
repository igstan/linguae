package toy

case class Store(bindings: Map[Int, Store.Cell], locationCount: Int) {
  def get(location: Store.Location): Option[Value] = {
    bindings.get(location).map(_.value.getOrElse {
      throw new IllegalStateException("Store.Cell not bound; this is a bug.")
    })
  }

  def set(location: Store.Location, value: Value): Store = {
    Store(bindings + (location -> new Store.Cell(Some(value))), locationCount)
  }

  def add(value: Value): (Store.Location, Store) = {
    val location = locationCount + 1
    val newBindings = bindings + (location -> new Store.Cell(Some(value)))
    location -> Store(newBindings, location)
  }

  def addUnbound: (Store.Location, Store.Cell, Store) = {
    val location = locationCount + 1
    val cell = new Store.Cell(None)
    val newBindings = bindings + (location -> cell)
    (location, cell, Store(newBindings, location))
  }

  def gc(old: Store): Store = {
    val oldKeys = old.bindings.keys.toSet
    val collected = bindings.filterKeys(oldKeys.contains(_))
    Store(collected, locationCount)
  }
}

object Store {
  type Location = Int

  class Cell(var value: Option[Value]) {
    def bind(value: Value): Unit = {
      this.value = Some(value)
    }

    override def toString = {
      value.map(_.toString).getOrElse("?")
    }
  }

  def empty = Store(Map.empty, 0)
}
