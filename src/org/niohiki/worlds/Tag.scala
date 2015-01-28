package org.niohiki.worlds

class Tag {
  def ->[T](key: PropertyLabel[T]): PropertyBin[T] =
    throw new OutsideOfWorldException
}
class OutsideOfWorldException
  extends Exception("Tags must be used within an inside clause")