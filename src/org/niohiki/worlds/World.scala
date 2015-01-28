package org.niohiki.worlds

import scala.collection.mutable.HashMap

class World(val parent: Option[World]) {
  private val tags = HashMap[Tag, HashMap[PropertyLabel[_], PropertyBin[_]]]()
  def get[T](tag: Tag, propertyLabel: PropertyLabel[T]): PropertyBin[T] = {
    getIfExists(tag, propertyLabel).getOrElse({
      tags.getOrElse(tag, {
        val newPropertyMap = HashMap[PropertyLabel[_], PropertyBin[_]]()
        tags.put(tag, newPropertyMap)
        newPropertyMap
      }).getOrElse(propertyLabel, { 
        val newPropertyBin = new PropertyBin[T](propertyLabel.default)
        tags(tag).put(propertyLabel,newPropertyBin)
        newPropertyBin
      }).asInstanceOf[PropertyBin[T]]
    })
  }
  private def getIfExists[T](tag: Tag,
                             property: PropertyLabel[T]): Option[PropertyBin[T]] = {
    tags.get(tag).
      map(_.get(property).asInstanceOf[Option[PropertyBin[T]]]).
      getOrElse(
        parent.flatMap(_.getIfExists(tag, property)))
  }
  override def toString = tags.toString
}