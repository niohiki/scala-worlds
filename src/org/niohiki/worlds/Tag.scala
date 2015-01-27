package org.niohiki.worlds

import scala.collection.mutable.HashMap

class Tag {
  private val properties = HashMap[Property[_],PropertyBin[_]]()
  def ->[T](key: Property[T]): PropertyBin[T] = {
    if(!properties.contains(key)){
      val newProperty = new PropertyBin[T](Some(key.default))
      properties += key -> newProperty
      newProperty
    } else {
      properties(key).asInstanceOf[PropertyBin[T]]
    }
  }
  def :>[T](key: Property[T]) = (this->key).apply()
}