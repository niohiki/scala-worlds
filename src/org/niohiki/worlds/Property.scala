package org.niohiki.worlds

abstract class PropertyLabel[+T] {
  def default: Option[T]
}
class PropertyBin[T](private var content: Option[T]){
  def :=(t:T) = content = Some(t)
  def apply():T = content.get
}
class TagProperty extends PropertyLabel[Tag]{ def default = Some(new Tag) }
class IntProperty extends PropertyLabel[Int]{ def default = None }
class StringProperty extends PropertyLabel[String]{ def default = None }