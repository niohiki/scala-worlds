package org.niohiki

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import com.sun.javafx.fxml.expression.Expression

package object worlds {
  def inside[A](w: World)(f: () => Unit): Unit = macro inside_impl

  def inside_impl(c: Context)(
    w: c.Expr[World])(f: c.Expr[() => Unit]): c.Expr[Unit] = {
    import c.universe._
    //val upgrade = reify{implicit val ww:World = w.splice} match {case c.Expr(x) => x}
    val upgrade = reify{println(w.splice.name)} match {case c.Expr(x) => x}
    val upgraded: c.Expr[() => Unit] = f match {
      case Expr(Function(_, block)) => block match {
        case Block(list, last) => c.Expr(Function(List(), Block(upgrade :: list, last)))
      }
    }
    //case Expr(Block((lis,b))) => c.Expr( Block( lis,b ) )
    //case b @ Expr(a) => b
    reify { (upgraded.splice)() }
  }
  implicit def property2value[T](property: PropertyBin[T]): T = property()
}