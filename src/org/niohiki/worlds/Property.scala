package org.niohiki.worlds

abstract class PropertyLabel[+T] {
  def default: Option[T]
}
abstract class PropertyBin[T] {
  def :=(t: T): Unit = {}
  def apply(): T
}
class TagProperty extends PropertyLabel[Tag] { def default = Some(new Tag) }
class IntProperty extends PropertyLabel[Int] { def default = None }
class StringProperty extends PropertyLabel[String] { def default = None }