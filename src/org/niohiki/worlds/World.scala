package org.niohiki.worlds

import scala.collection.mutable.HashMap

class World(val parent: Option[World]) {
  private val tags = HashMap[Tag, HashMap[PropertyLabel[_], PropertyBin[_]]]()
  def set[T](property: PropertyBin[T], value: T): Unit = {
    val propertyBin = property.asInstanceOf[PropertyBinImplementation[T]]
    getInLocal(propertyBin.parent, propertyBin.label).
      asInstanceOf[PropertyBinImplementation[T]].content = Some(value)
  }
  def get[T](tag: Tag, propertyLabel: PropertyLabel[T]): PropertyBin[T] = {
    getIfExists(tag, propertyLabel).getOrElse(getInLocal(tag, propertyLabel))
  }
  private def getInLocal[T](tag: Tag,
                            propertyLabel: PropertyLabel[T]): PropertyBin[T] = {
    tags.getOrElse(tag, {
      val newPropertyMap = HashMap[PropertyLabel[_], PropertyBin[_]]()
      tags.put(tag, newPropertyMap)
      newPropertyMap
    }).getOrElse(propertyLabel, {
      val newPropertyBin = new PropertyBinImplementation[T](tag,
        propertyLabel, propertyLabel.default)
      tags(tag).put(propertyLabel, newPropertyBin)
      newPropertyBin
    }).asInstanceOf[PropertyBin[T]]
  }
  private def getIfExists[T](tag: Tag,
                             property: PropertyLabel[T]): Option[PropertyBin[T]] = {
    val local = tags.get(tag).
      flatMap(_.get(property).asInstanceOf[Option[PropertyBin[T]]])
    if (local.isDefined) local else parent.flatMap(_.getIfExists(tag, property))
  }
  override def toString = tags.toString

  private class PropertyBinImplementation[T](val parent: Tag,
                                             val label: PropertyLabel[T],
                                             var content: Option[T])
    extends PropertyBin[T] {
    def apply() = content.get
  }
}