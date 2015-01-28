package org.niohiki

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object worlds {
  def inside[A](w: World)(f: => Unit): Unit = macro inside_impl
  def inside_impl(c: Context)(
    w: c.Expr[World])(f: c.Expr[_]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(parseGetSet(c)(w, f.tree))
  }

  def parseGetSet(c: Context)(w: c.Expr[World],
                              t: c.universe.Tree): c.universe.Tree = {
    val getT = newGetTransfomer(c)(w)
    val setT = newSetTransfomer(c)(w)
    c.untypecheck(setT.transform(getT.transform(t)))
  }

  def newGetTransfomer(c: Context)(w: c.Expr[World]) = new c.universe.Transformer {
    import c.universe._
    override def transform(tree: Tree): Tree = {
      tree match {
        case Apply(
          TypeApply(Select(identifier, TermName("$minus$greater")), typeList),
          List(property)) if identifier.tpe <:< c.typeOf[Tag] =>
          Apply(
            TypeApply(Select(w.tree, TermName("get")), typeList),
            List(identifier, property))
        case other => super.transform(other)
      }
    }
  }

  def newSetTransfomer(c: Context)(w: c.Expr[World]) = new c.universe.Transformer {
    import c.universe._
    override def transform(tree: Tree): Tree = {
      tree match {
        case Apply(Select(identifier, TermName("$colon$eq")), List(value)) =>
          Apply(
            TypeApply(Select(w.tree, TermName("set")), List(TypeTree())),
            List(identifier, value))
        case other => super.transform(other)
      }
    }
  }

  def inspect(e: Any): Unit = macro inspect_impl
  def inspect_impl(c: Context)(e: c.Expr[_]): c.Expr[Unit] = {
    import c.universe._
    print_impl(c)(showRaw(e))
  }

  def print_impl(c: Context)(a: Any): c.Expr[Unit] = {
    import c.universe._
    val e = c.Expr(Literal(Constant(a)))
    reify { println(e.splice) }
  }

  implicit def property2value[T](property: PropertyBin[T]): T = property()
}