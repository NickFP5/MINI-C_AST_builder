/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package scalaapplication1

import scala.util.parsing.combinator._

class PaganoParser extends JavaTokenParsers{

  //var tree : Node[String]
  /*def obj: Parser[Map[String, Any]] =
    "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
  
  def arr: Parser[List[Any]] =
    "["~> repsep(value, ",") <~"]"
  
  def member: Parser[(String, Any)] =
    stringLiteral~":"~value ^^
  { case name~":"~value => (name, value) }
  
  def value: Parser[Any] = (
    obj
    | arr
    | stringLiteral
    | floatingPointNumber ^^ (_.toDouble)
    | "null" ^^ (x => null)
    | "true" ^^ (x => true)
    | "false" ^^ (x => false)
  )*/
  
  def program: Parser[AbstractSyntaxTree[String]] = {
    //var result: String = "(program"
    //tree = Node("Program", List())
    
    "{"~> rep(stmt) <~ "}" ^^ 
    { 
      case ms => Node("Program", ms.toList)
        /*ms.foreach(tree.addToChildren(_))
                  tree*/
    }
    //result + ")"
    
  }
  
  def stmt: Parser[AbstractSyntaxTree[String]] =
    ( simp ~ ";" | control | ";" ) ^^
    { 
      case Node(elem, nodes_list) ~ ";" => Node(elem.toString, nodes_list.asInstanceOf[List[AbstractSyntaxTree[String]]]) 
      case Node(elem, nodes_list) => Node(elem.toString, nodes_list.asInstanceOf[List[AbstractSyntaxTree[String]]])
      //case _ => ""
    }
  
  def simp: Parser[AbstractSyntaxTree[String]] =
    ( identifier ~ asop ~ expr ) ^^ 
    {
      case stringLiteral ~ Leaf(elem) ~ expr => Node(elem.toString , List(Leaf(stringLiteral), expr))
    }
    /*{
      case ident => ident
      case asop => asop
      case expr => expr
    }*/
  
  /*def control: Parser[AbstractSyntaxTree[String]] =
    ( "if"~"("~expr~")"~block~opt("else"~block) | "while"~"("~expr~")"~block ) ^^
    {
      //case "if"~"("~expr~")"~block1~"else"~block2 => Node("if-else", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block1.asInstanceOf[AbstractSyntaxTree[String]], block2.asInstanceOf[AbstractSyntaxTree[String]]) )
      case "if"~"("~expr~")"~block => Node("if", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block.asInstanceOf[AbstractSyntaxTree[String]], block.asInstanceOf[AbstractSyntaxTree[String]]) )
      case "while"~"("~expr~")"~block => Node("while", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block.asInstanceOf[AbstractSyntaxTree[String]]))
    }*/
   
  def control: Parser[AbstractSyntaxTree[String]] =
    ( "if"~"("~expr~")"~block~opt("else"~>block) | "while"~"("~expr~")"~block ) ^^
    {
      case "if"~"("~expr~")"~block~None => 
        Node("if", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block.asInstanceOf[AbstractSyntaxTree[String]] ) )
        //else Node("if-else", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block1.asInstanceOf[AbstractSyntaxTree[String]], block2.asInstanceOf[AbstractSyntaxTree[String]]) )
      case "if"~"("~expr~")"~block1~"else"~block2 => Node("if-else", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block1.asInstanceOf[AbstractSyntaxTree[String]], block2.asInstanceOf[AbstractSyntaxTree[String]]) )
      case "while"~"("~expr~")"~block => Node("while", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block.asInstanceOf[AbstractSyntaxTree[String]]))
    }
  
  def block: Parser[AbstractSyntaxTree[String]] =
    stmt | ("{"~>rep(stmt)<~"}") ^^
    { 
      case ms => 
        if(ms.length > 1)Node("block", ms.toList)
        else Node("block", ms)
      //case stmt => `stmt`.last
    }
  
  /* !!!!!!!!!!!! RICORSIONE SINISTRA !!!!!!!!!!!!!!*/
  /*def expr: Parser[AbstractSyntaxTree[String]] = //intconst = decimalNumber
    ( "("~expr~")" | unop~expr | expr~binop~expr | intconst | identifier ) ^^
    {
      case "("~expr~")" => expr.asInstanceOf[AbstractSyntaxTree[String]]
      case Leaf(elem)~expr => Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case expr1~Leaf(elem)~expr2 => Node(elem.toString, List(expr1.asInstanceOf[AbstractSyntaxTree[String]], expr2.asInstanceOf[AbstractSyntaxTree[String]]))
      case intconst => Leaf(intconst.toString)
      case identifier => Leaf(identifier.toString)
  }*/
 
  def expr: Parser[AbstractSyntaxTree[String]] = //intconst = decimalNumber
    ( "("~expr~")"~binop_expr | unop~expr~binop_expr | intconst~binop_expr | identifier~binop_expr ) ^^
    {
      case "("~expr~")"~Node(e, l) => 
        if(e.toString != "")Node(e, expr.asInstanceOf[AbstractSyntaxTree[String]] :: l)
        else expr.asInstanceOf[AbstractSyntaxTree[String]]
      case Leaf(elem)~expr~Node(e, l) => 
        if(e.toString != "")Node(e, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l )
        else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      //case expr1~Leaf(elem)~expr2 => Node(elem.toString, List(expr1.asInstanceOf[AbstractSyntaxTree[String]], expr2.asInstanceOf[AbstractSyntaxTree[String]]))
      case intconst~Node(e, l) => 
        if(e.toString != "")Node(e, Leaf(intconst.toString) :: l)
        else Leaf(intconst.toString)
      case identifier~Node(e, l) => 
        if(e.toString != "")Node(e, Leaf(identifier.toString) :: l)
        else Leaf(identifier.toString)
  }
  
  def binop_expr: Parser[AbstractSyntaxTree[String]] =
    (binop~expr~binop_expr | "") ^^
    {
      //case Leaf(elem)~expr~"" => Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case Leaf(elem)~expr~Node(e, l) => 
        if(e.toString != "") Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
        else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case "" => Node("", List())
    }
  
  def asop: Parser[AbstractSyntaxTree[String]] =
    "=" ^^
    {case "=" => Leaf("=")}
  
  def unop: Parser[Leaf[String]] =
    ("!" | "-") ^^
    {
      case "!" => Leaf("!")
      case "-" => Leaf("-")
    }
  
  def binop: Parser[Leaf[String]] =
    ("+" | "-" | "*" | "/" | "%" | "<" | "<=" | ">" | ">=" | "==" | "!=" | "&&" | "||") ^^
    {
      case "+" => Leaf("+")
      case "-" => Leaf("-")
      case "*" => Leaf("*")
      case "/" => Leaf("/")
      case "%" => Leaf("%")
      case "<" => Leaf("<")
      case "<=" => Leaf("<=")
      case ">" => Leaf(">")
      case ">=" => Leaf(">=")
      case "==" => Leaf("==")
      case "!=" => Leaf("!=")
      case "&&" => Leaf("&&")  
      case "||" => Leaf("||")
    }
  
  def identifier: Parser[String] =
    "[A-Za-z][0-9A-Za-z]*".r
  
  def intconst: Parser[String] =
    "[0-9][0-9]*".r
  
}