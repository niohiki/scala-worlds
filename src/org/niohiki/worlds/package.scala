package org.niohiki

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.TypecheckException

package object worlds {
  def branch[T](inputWorld: World)(f: => T): T = macro branch_impl
  def branch_impl(c: Context)(inputWorld: c.Tree)(f: c.Tree): c.Tree = {
    import c.universe._
    q"""inside($inputWorld.branch("branch"))($f)"""
  }
  def inside[T](inputWorld: World)(f: => T): T = macro inside_impl
  def inside_impl(c: Context)(inputWorld: c.Tree)(f: c.Tree): c.Tree = {
    import c.universe._
    def treeTransform(t: Tree, f: Tree => Tree): Tree = {
      c.untypecheck(new Transformer {
        override def transform(tree: Tree): Tree = {
          f(super.transform(tree))
        }
      }.transform(c.typecheck(t)))
    }
    val worldIdentifier = Ident(TermName(c.freshName("internalWorld")))
    val tokenSymbol = c.typecheck(q"localWorld").symbol
    val getValueTree = treeTransform(f, _ match {
      case q"$tag-<[$types]($property)" if c.typecheck(
        tag).tpe <:< c.typeOf[Tag] =>
        q"$tag.->[$types]($property).apply()"
      case default => default
    })
    val getTree = treeTransform(getValueTree, _ match {
      case q"$tag->[$types]($property)" if c.typecheck(
        tag).tpe <:< c.typeOf[Tag] =>
        q"localWorld.get[$types]($tag,$property)"
      case q"$property:=($value)" if c.typecheck(
        property).tpe <:< c.typeOf[PropertyBin[_]] =>
        q"localWorld.set($property,$value)"
      case default => default
    })
    val tokenTree = treeTransform(getTree, _ match {
      case item => {
        val symbol = c.typecheck(item, silent = true).symbol
        if (symbol != null && symbol.fullName.equals(tokenSymbol.fullName)) {
          worldIdentifier
        } else item
      }
    })
    q"val $worldIdentifier=$inputWorld;$tokenTree"
  }

  def inspect(e: Any): Unit = macro inspect_impl
  def inspect_impl(c: Context)(e: c.Tree): c.Tree = {
    import c.universe._
    print_impl(c)(showRaw(e))
  }

  def print_impl(c: Context)(a: Any): c.Tree = {
    import c.universe._
    val e = a.toString
    q"println($e)"
  }

  implicit def property2value[T](property: PropertyBin[T]): T = property()
  implicit val localWorld = new World("local")
}