package main.scala

object Repl {

  def main(args: Array[String]): Unit = {
    repl()
  }

  def repl(): Unit = {
    val input = scala.io.StdIn.readLine("db > ")
    input match {
      case null =>
        print("Error while reading. No input to read")
        repl()
      case ".exit" =>
      case _ =>
        println("Invalid command")
        repl()
    }
  }
}

