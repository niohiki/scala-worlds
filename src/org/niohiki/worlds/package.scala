package org.niohiki

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

package object worlds {
  def inside[A](w: World)(f: => Unit): Unit = macro inside_impl
  def inside_impl(c: Context)(
    w: c.Expr[World])(f: c.Expr[_]): c.Expr[Unit] = {
    import c.universe._

    def treeTransform(f: Tree => Tree, t: Tree): Tree = {
      c.untypecheck(new Transformer {
        override def transform(tree: Tree): Tree = {
          f(super.transform(tree))
        }
      }.transform(t))
    }

    val tokenSymbol = c.typecheck(reify { localWorld }.tree).symbol
    val resTree = treeTransform(_ match {
      case x => {
        try {
          val symbol = c.typecheck(x).symbol
          if (symbol != null && symbol.fullName.equals(tokenSymbol.fullName)) {
            w.tree
          } else {
            x
          }
        } catch {
          case ex: TypecheckException => x
        }
      }
    }, treeTransform(_ match {
      case Apply(
        TypeApply(Select(identifier, TermName("$minus$greater")), typeList),
        List(property)) if c.typecheck(identifier).tpe <:< c.typeOf[Tag] => {
        Apply(
          TypeApply(Select(w.tree, TermName("get")), typeList),
          List(identifier, property))
      }
      case Apply(
        TypeApply(Select(identifier, TermName("$minus$less")), typeList),
        List(property)) if c.typecheck(identifier).tpe <:< c.typeOf[Tag] => {
        val gettingExp: c.Expr[PropertyBin[_]] = c.Expr(Apply(
          TypeApply(Select(w.tree, TermName("get")), typeList),
          List(identifier, property)))
        reify { (gettingExp.splice)() }.tree
      }
      case Apply(Select(identifier, TermName("$colon$eq")),
        List(value)) if c.typecheck(identifier).tpe <:< c.typeOf[PropertyBin[_]] =>
        Apply(
          TypeApply(Select(w.tree, TermName("set")), List(TypeTree())),
          List(identifier, value))
      case other => other
    }, f.tree))
    c.Expr(resTree)
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
  implicit val localWorld = new World
}