package org.niohiki

package object worlds {
  def inside[A](w: World)(f: =>Unit){
    f
  }
  implicit def property2value[T](property: PropertyBin[T]):T = property()
}