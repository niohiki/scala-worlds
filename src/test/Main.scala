package test

import org.niohiki.worlds._

object Main {
  def main(args:Array[String]){
    println("hi")
    val w = new World
    inside(w) {
      println("hoo")
    }
    val ss = new Tag
    val name = new Property[String]{ def default = ""}
    val age = new Property[Int]{ def default = 0}
    val car = new Property[Tag]{ def default = new Tag}
    ss->name := "jajaja"
    ss->age := 5
    ss->car->name:= "hi ho hu"
    println(ss->car:>name)
  }
}