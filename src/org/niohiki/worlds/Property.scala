package org.niohiki.worlds

abstract class Property[+T] {
  def default: T
}
class PropertyBin[T](private var content: Option[T]){
  def :=(t:T) = content = Some(t)
  def apply():T = content.get
}