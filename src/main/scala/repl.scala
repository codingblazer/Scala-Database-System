package main.scala

import main.scala.StatementTypes.{INSERT, SELECT}

import scala.io.StdIn.readLine

trait StatementType

object StatementTypes {
  case object SELECT extends StatementType
  case object INSERT extends StatementType
}

case class ParsedStatement(statement: String, isSuccess: Boolean, stateType: Option[StatementType])

object Repl {

  def main(args: Array[String]): Unit = {
    repl()
  }

  def repl(): Unit = {
    val input = readLine("db > ")
    input match {
      case null =>
        print("Error : No input to read. Exiting repl...")
      case x if x.charAt(0) == '.' =>
        handleMetaCommand(input)
      case _ =>
        val parsedStatement: ParsedStatement = prepareStatement(input)
        parsedStatement.isSuccess match {
          case true =>
            executeStatement(parsedStatement)
          case false =>
            println("Cannot parse as illegal start of statement. Try again...")
            repl()
        }
    }
  }

  def handleMetaCommand(input: String) = {
    input match {
      case ".exit" =>
        println("Exiting repl...")
      case _ =>
        println("Invalid meta command. Try again...")
        repl()
    }
  }

  def prepareStatement(input: String) : ParsedStatement = {
    input match {
      case statement if statement.length > 6 && statement.substring(0,6).toLowerCase == "select" => ParsedStatement(input, true, Some(SELECT))
      case statement if statement.length > 6 && statement.substring(0,6).toLowerCase == "insert" => ParsedStatement(input, true, Some(INSERT))
      case _ => ParsedStatement(input, false, None)
    }
  }

  def executeStatement(parsedStatement: ParsedStatement) = {
    parsedStatement.stateType match {
      case Some(SELECT) =>
        println("Executing select statement...")
        repl()
      case Some(INSERT) =>
        println("Executing insert statement...")
        repl()
      case _ =>
        println("Unrecognized statement, can't execute")
        repl()
    }
  }

}

