package test

import org.niohiki.worlds._

object Main {
  def hola(s: String)(implicit w: World):Unit = println(s+" "+w)
  def main(args:Array[String]){
    println("hi")
    val w = new World
    inside(w) {
      println("hoo")
    }
    val ss = new Tag
    val name = new StringProperty
    val age = new IntProperty
    val car = new TagProperty
    ss->name := "jajaja"
    ss->age := 5
    ss->car->name:= "hi ho hu"
    println(ss->car:>name)
  }
}