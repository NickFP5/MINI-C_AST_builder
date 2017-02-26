/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package scalaapplication1

object Main extends PaganoParser{

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println(parseAll(program, "{if(b == 3)a = 3;else e = 0; c = 2; while(!c) d = 1;}"))
  }

}
