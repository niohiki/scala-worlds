package org.niohiki.worlds

import scala.collection.mutable.HashMap

object Tag {
  implicit val baseWorld: World = null
}

class Tag {
  private val properties = HashMap[Property[_],PropertyBin[_]]()
  def ->[T](key: Property[T])(implicit world: World): PropertyBin[T] = {
    if(!properties.contains(key)){
      val newProperty = new PropertyBin[T](Some(key.default))
      properties += key -> newProperty
      newProperty
    } else {
      properties(key).asInstanceOf[PropertyBin[T]]
    }
  }
  def :>[T](key: Property[T])(implicit world:World) = (this->key).apply()
}