package org.niohiki.worlds

abstract class Property[+T] {
  def default: T
}
class PropertyBin[T](private var content: Option[T]){
  def :=(t:T) = content = Some(t)
  def apply():T = content.get
}
class TagProperty extends Property[Tag]{ def default = new Tag }
class IntProperty extends Property[Int]{ def default = 0 }
class StringProperty extends Property[String]{ def default = "" }