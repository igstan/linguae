package toy

case class Store(bindings: Map[Store.Location, Value], locationCount: Int) {
  def get(location: Store.Location): Option[Value] = {
    bindings.get(location)
  }

  def set(location: Store.Location, value: Value): Store = {
    Store(bindings + (location -> value), locationCount)
  }

  def add(value: Value): (Store.Location, Store) = {
    val location = locationCount + 1
    val newBindings = bindings + (location -> value)
    location -> Store(newBindings, location)
  }
}

object Store {
  type Location = Int
  def empty = Store(Map.empty, 0)
}
