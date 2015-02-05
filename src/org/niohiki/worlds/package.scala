package org.niohiki

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

package object worlds {
  def lulz(x: Any): Unit = macro lulz_impl
  def lulz_impl(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._

    val ident = Ident(TermName(c.freshName("dfsf")))

    q"""{
          val $ident = $x
          println($ident)
        }"""

    x match {
      case q"$iden.$met($thin)" => {
        val nam: String = thin.toString
        val num = 5
        val two = 1 + 1
        val four = q"$num+$two"
        q"""println($nam)"""
      }
    }
  }

  def inside(inputWorld: World)(f: => Unit): Unit = macro inside_impl
  def inside_impl(c: Context)(inputWorld: c.Tree)(f: c.Tree): c.Tree = {
    import c.universe._
    def treeTransform(t: Tree, f: Tree => Tree): Tree = {
      c.untypecheck(new Transformer {
        override def transform(tree: Tree): Tree = {
          f(super.transform(tree))
        }
      }.transform(t))
    }

    val tokenSymbol = (q"localWorld").symbol
    val returnTree = treeTransform(f, _ match {
      case q"$tag->[$types]($property)" if c.typecheck(
        tag).tpe <:< c.typeOf[Tag] =>
        q"$inputWorld.get[$types]($tag,$property)"
      case q"$tag-<[$types]($property)" if c.typecheck(
        tag).tpe <:< c.typeOf[Tag] =>
        q"($inputWorld.get[$types]($tag,$property))()"
      case q"$property:=($value)" => q"$inputWorld.set($property,$value)"
      case other => {
        val symbol = c.typecheck(other, silent = true).symbol
        if (symbol != null && symbol.fullName.equals(tokenSymbol.fullName)) {
          inputWorld
        } else other
      }
    })
    returnTree
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