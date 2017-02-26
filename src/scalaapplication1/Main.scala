/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package scalaapplication1

import scala.io.Source
import java.io.File

object Main extends PaganoParser{

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    println("Welcome to MINI-C AST Builder!")
    println("Student: Pagano Niccolò Fabrizio O55000281")
    //println(parseAll(program, "{if(b == 3)a = 3;else e = 0; c = 2; while(!c) d = 1;}"))
    
    //val file = new File("prova.txt")
    //file.createNewFile();
    val filenames = List("simple_if.txt", "simple_if_else.txt", 
                         "simple_while.txt", "test_file_1.txt",
                         "test_file_2.txt", "while_as_identifier_error.txt",
                         "if_as_identifier_error.txt", "else_as_identifier_error.txt")
    
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
        case ex: Exception => println("A file exception happened.")
      }
    }
  }

}
