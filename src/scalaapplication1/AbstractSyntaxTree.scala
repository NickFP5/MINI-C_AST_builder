/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package scalaapplication1

abstract class AbstractSyntaxTree[+T]
  case class Leaf[T](elem: T) extends AbstractSyntaxTree[T]{
    override def toString: String = "("+elem+")"
  }
  /*{
    def content = elem
  }*/
  case class Node[T](elem: T, children: List[AbstractSyntaxTree[T]]) extends AbstractSyntaxTree[T]{
    override def toString: String = {
      var str: String = "("+elem.toString
      children.foreach{e =>
                       //str+="("
                       str+=e
                       //str+=")"
      }
      str+=")"
      str
    }
  }
  /*{
    children = List()
    
    def addToChildren(t: AbstractSyntaxTree[T])
      this.children :: t
  }*/

