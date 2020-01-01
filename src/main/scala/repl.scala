package main.scala

import main.scala.ParsedStatementTypes.{FailureParsedStatement, InsertParsedStatement, SelectParsedStatement}

import scala.io.StdIn.readLine

trait ParsedStatement {
  def statement: String
}

case class Page(rows: List[Row])

case class Table(numRows: Int, rows: List[Row])

object ParsedStatementTypes {

  case class FailureParsedStatement(statement: String, reason: String) extends ParsedStatement

  case class SelectParsedStatement(statement: String) extends ParsedStatement

  case class InsertParsedStatement(statement: String, rowToInsert: Row) extends ParsedStatement

}

case class Row(id: Int, userName: String, email: String)

object Repl {

  val MAX_TABLE_ROWS = 5

  def main(args: Array[String]): Unit = {
    val initialTable = Table(0, List[Row]())
    repl(initialTable)
  }

  def repl(table: Table): Unit = {
    val input = readLine("db > ")
    input match {
      case null =>
        print("Error : No input to read. Exiting repl...")
      case x if x.charAt(0) == '.' =>
        handleMetaCommand(input, table)
      case _ =>
        val parsedStatement: ParsedStatement = prepareStatement(input)
        parsedStatement match {
          case failure: FailureParsedStatement =>
            println(failure.reason)
            repl(table)
          case _: ParsedStatement => executeStatement(parsedStatement, table)

        }
    }
  }

  def handleMetaCommand(input: String, table: Table) = {
    input match {
      case ".exit" =>
        println("Exiting repl...")
      case _ =>
        println("Invalid meta command. Try again...")
        repl(table)
    }
  }

  def prepareStatement(input: String): ParsedStatement = {
    input match {
      case statement if statement.length > 6 && statement.substring(0, 6).toLowerCase == "select" =>
        SelectParsedStatement(input)
      case statement if statement.length > 6 && statement.substring(0, 6).toLowerCase == "insert" =>
        val rowAsArray = statement.split(" ")
        if (rowAsArray.size < 4) FailureParsedStatement(statement, "SYNTAX ERROR : Insert statement does not have all the requirement arguments")
        else InsertParsedStatement(input, Row(rowAsArray(1).toInt, rowAsArray(2), rowAsArray(3)))
      case _ => FailureParsedStatement(input, "Cannot parse as illegal start of statement. Try again...")
    }
  }

  def executeStatement(parsedStatement: ParsedStatement, table: Table) = {
    parsedStatement match {
      case selectStatement: SelectParsedStatement =>
        println("Executing select statement...")
        table.rows.foreach(row => println(row.id + " " + row.userName + " " + row.email))
        println(s"Execute Success : Printed ${table.numRows} rows")
        repl(table)
      case insertStatement: InsertParsedStatement =>
        println("Executing insert statement...")
        if(table.numRows < MAX_TABLE_ROWS){
          val updatedTable = table.copy(numRows = table.numRows + 1, table.rows :+ insertStatement.rowToInsert)
          println("Execute Success")
          repl(updatedTable)
        } else {
          println("Execute Failure : Table is full")
          repl(table)
        }
      case _: ParsedStatement =>
        println("Unrecognized statement, can't execute")
        repl(table)
    }
  }

}