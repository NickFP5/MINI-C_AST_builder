/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * author: Nick F Pagano
 */

package Mini_C_AST_Builder

import scala.io.Source
import java.io.File

object Main extends MINICParser{

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {

    val filenames = List("simple_if.txt", "simple_if_else.txt", 
                         "simple_while.txt", "test_file_1.txt",
                         "test_file_2.txt", "while_as_identifier_error.txt",
                         "if_as_identifier_error.txt", "else_as_identifier_error.txt",
                         "test_file_3.txt", "combined_expression_1.txt",
                         "combined_expression_2.txt", "combined_expression_3.txt")
    
    println("Welcome to MINI-C AST Builder!")
    println("Student: Pagano Niccolò Fabrizio O55000281")
    
    for(filename <- filenames){
      var program_to_parse: String = ""
      println("\nTrying to parse file: " + filename)
      try {
        for (line <- Source.fromFile(filename).getLines()) {
          program_to_parse += (line+"\n")
        }
        println("File content: " + program_to_parse)
        println(parseAll(program, program_to_parse ))
      } catch {
        case ex: Exception => println("A exception happened.")
      }
    }
  }

}
