package org.niohiki.worlds

import scala.collection.mutable.HashMap

object Tag {
  implicit val baseWorld: World = null
}

class Tag {
  private val properties = HashMap[Property[_],PropertyBin[_]]()
  def getIn[T](key: Property[T],world: World): PropertyBin[T] = {
    println(world.name)
    if(!properties.contains(key)){
      val newProperty = new PropertyBin[T](Some(key.default))
      properties += key -> newProperty
      newProperty
    } else {
      properties(key).asInstanceOf[PropertyBin[T]]
    }
  }
  def ->[T](key: Property[T]): PropertyBin[T] = getIn(key,null)
}