/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package scalaapplication1

import scala.util.parsing.combinator._

class PaganoParser extends JavaTokenParsers{

  //override val whiteSpace = "(/*.*/)|(//.\\n)|\\t|".r
  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  //var tree : Node[String]
  val precedences = Map( //ordering: highest to lowest
                        "(" -> 9, 
                        ")" -> 9,
                        "!" -> 8,
                        "*" -> 7,
                        "/" -> 7,
                        "%" -> 7,
                        "+" -> 6,
                        "-" -> 6,
                        "<" -> 5,
                        "<=" -> 5,
                        ">" -> 5,
                        ">=" -> 5,
                        "==" -> 4,
                        "!=" -> 4,
                        "&&" -> 3,
                        "||" -> 2,
                        "=" -> 1
                       )
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
    asop_expr
    /*( identifier ~ asop ~ expr ) ^^ 
    {
      case stringLiteral ~ Leaf(elem) ~ expr => Node(elem.toString , List(Leaf(stringLiteral), expr))
    }*/
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
      case "if"~"("~expr~")"~block1~Some(block2) => Node("if-else", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block1.asInstanceOf[AbstractSyntaxTree[String]], block2.asInstanceOf[AbstractSyntaxTree[String]]) )
      case "if"~"("~expr~")"~block~None => 
        Node("if", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block.asInstanceOf[AbstractSyntaxTree[String]] ) )
        //else Node("if-else", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block1.asInstanceOf[AbstractSyntaxTree[String]], block2.asInstanceOf[AbstractSyntaxTree[String]]) )
      //case "if"~"("~expr~")"~block1~"else"~block2 => Node("if-else", List(expr.asInstanceOf[AbstractSyntaxTree[String]], block1.asInstanceOf[AbstractSyntaxTree[String]], block2.asInstanceOf[AbstractSyntaxTree[String]]) )
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
    (binop_expr | "("~expr~")") ^^
    {
      case "("~exp~")" => exp.asInstanceOf[AbstractSyntaxTree[String]]
      case Leaf(e) => Leaf(e.toString)
      case Node(e, l) => Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]])
    }
    /*( "("~expr~")"~binop_expr | unop~expr~binop_expr | intconst~binop_expr | identifier~binop_expr ) ^^
    {
      case "("~expr~")"~Node(e, l) => 
        if(e.toString != "")Node(e, expr.asInstanceOf[AbstractSyntaxTree[String]] :: l)
        else expr.asInstanceOf[AbstractSyntaxTree[String]]
      case Leaf(elem)~expr~Node(e, l) => 
        if(e.toString != "")Node(e, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l )
          //if(this.precedences(elem.toString) > this.precedences(e.toString)) Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          //else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]], Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]]) )) 
        else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      /*case Leaf(elem)~expr => 
        Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))*/
      //case expr1~Leaf(elem)~expr2 => Node(elem.toString, List(expr1.asInstanceOf[AbstractSyntaxTree[String]], expr2.asInstanceOf[AbstractSyntaxTree[String]]))
      /*case decimalNumber~Node(e, l) => 
        if(e.toString != "")Node(e, Leaf(decimalNumber.toString) :: l)
        else Leaf(intconst.toString)*/
      case stringLiteral~Node(e, l) => 
        if(e.toString != "")Node(e, Leaf(stringLiteral.toString) :: l)
        else Leaf(stringLiteral.toString)
    }*/
  
  /*def binop_expr: Parser[AbstractSyntaxTree[String]] =
    (binop~expr~binop_expr | "") ^^
    {
      //case Leaf(elem)~expr~"" => Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case Leaf(elem)~expr~Node(e, l) => 
        if(e.toString != "") //Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          if(this.precedences(elem.toString) > this.precedences(e.toString))Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]], Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]]) )) 
        else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case "" => Node("", List())
    }*/
   
  def term: Parser[AbstractSyntaxTree[String]] =
    ("("~expr~")" | intconst | identifier ) ^^
    {
      case "("~expr~")" => expr.asInstanceOf[AbstractSyntaxTree[String]]
      case stringLit => Leaf(stringLit.toString)
    }
    
  def unary_expr: Parser[AbstractSyntaxTree[String]] =
    (unop~term) ^^ 
    {
      case Leaf(e1)~Node(e2, l) => Node(e1.toString, List(Node(e2,l)))
      case Leaf(e1)~Leaf(e2) => Node(e1.toString, List(Node(e2,List())))
    }
  
  def mult_expr: Parser[AbstractSyntaxTree[String]] =
    //(term~multop~term | term~multop~unary_expr | term) ^^
    (term~multop~mult_expr | term~multop~unary_expr | term) ^^
    {
      case Node(e, l) => Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]])
      case Leaf(e) => Leaf(e.toString)
      case t~Leaf(op)~n => Node(op.toString, List(t.asInstanceOf[AbstractSyntaxTree[String]], n.asInstanceOf[AbstractSyntaxTree[String]]))
    }
    
  def additive_expr: Parser[AbstractSyntaxTree[String]] =
    //(mult_expr~addop~mult_expr | mult_expr ) ^^
    (mult_expr~addop~additive_expr | mult_expr ) ^^
    {
      case t~Leaf(op)~n => Node(op.toString, List(t.asInstanceOf[AbstractSyntaxTree[String]], n.asInstanceOf[AbstractSyntaxTree[String]]))
      case Leaf(e) => Leaf(e.toString)
      case Node(e, l) => Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]])
  }
    
  def rel_expr: Parser[AbstractSyntaxTree[String]] =
    (additive_expr~relop~additive_expr) ^^
    {
      case n1~Leaf(e)~n2 => Node(e.toString, List(n1.asInstanceOf[AbstractSyntaxTree[String]], n2.asInstanceOf[AbstractSyntaxTree[String]]))
    }
    
  def equality_expr: Parser[AbstractSyntaxTree[String]] =
    (additive_expr~equalop~additive_expr) ^^
    {
      case n1~Leaf(e)~n2 => Node(e.toString, List(n1.asInstanceOf[AbstractSyntaxTree[String]], n2.asInstanceOf[AbstractSyntaxTree[String]]))
    }
    
  def logic_inner_expr: Parser[AbstractSyntaxTree[String]] =
    (equality_expr | rel_expr) ^^
    {
      case Node(e, l) => Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]])
    }
  
  def and_logic_expr: Parser[AbstractSyntaxTree[String]] =
    (logic_inner_expr~"&&"~logic_inner_expr) ^^
    {
      case l1~"&&"~l2 => Node("&&", List(l1.asInstanceOf[AbstractSyntaxTree[String]], l2.asInstanceOf[AbstractSyntaxTree[String]]))
    }
  
  def or_logic_inner_expr: Parser[AbstractSyntaxTree[String]] =
    (logic_inner_expr | and_logic_expr) ^^
    {
      case Node(e, l) => Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]])
    }
    
  def or_logic_expr: Parser[AbstractSyntaxTree[String]] =
    (or_logic_inner_expr~"||"~or_logic_inner_expr)^^
    {
      case l1~"||"~l2 => Node("||", List(l1.asInstanceOf[AbstractSyntaxTree[String]], l2.asInstanceOf[AbstractSyntaxTree[String]]))
    }
    
  def asop_inner_expr: Parser[AbstractSyntaxTree[String]] =
    (or_logic_expr | and_logic_expr | equality_expr | rel_expr | additive_expr | mult_expr | unary_expr)
  
  def asop_expr: Parser[AbstractSyntaxTree[String]] =
    (identifier~"="~asop_inner_expr)^^
    {
      case stringLit~"="~n => Node("=", List(Leaf(stringLit.toString), n.asInstanceOf[AbstractSyntaxTree[String]]))
    }
  
  def binop_expr: Parser[AbstractSyntaxTree[String]] =
    (or_logic_expr | and_logic_expr | equality_expr | rel_expr | additive_expr | mult_expr | unary_expr)
    /*(binop~expr~binop_expr | "") ^^
    {
      //case Leaf(elem)~expr~"" => Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case Leaf(elem)~Leaf(identif)~Node(e, l) =>
        if(e.toString != "") //Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          if(this.precedences(elem.toString) > this.precedences(e.toString))Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]], Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]]) )) 
        else Node(elem.toString, List(Leaf(identif.toString)))
      case Leaf(elem)~Node(el, li)~Node(e, l) => 
        if(e.toString != "") //Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          if(this.precedences(elem.toString) > this.precedences(e.toString))Node(e.toString, Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]])) :: l.asInstanceOf[List[AbstractSyntaxTree[String]]])
          else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]], Node(e.toString, l.asInstanceOf[List[AbstractSyntaxTree[String]]]) )) 
        else Node(elem.toString, List(expr.asInstanceOf[AbstractSyntaxTree[String]]))
      case "" => Node("", List())
    }*/
  
  def asop: Parser[AbstractSyntaxTree[String]] =
    "=" ^^
    {case "=" => Leaf("=")}
  
  def unop: Parser[Leaf[String]] =
    ("!" | "-") ^^
    {
      case "!" => Leaf("!")
      case "-" => Leaf("-")
    }
    
  def multop: Parser[Leaf[String]] =
    ("*" | "/" | "%") ^^
    {
      case stringLit => Leaf(stringLit.toString)
    }
   
  def addop: Parser[Leaf[String]] =
    ("+" | "-") ^^
    {
      case stringLit => Leaf(stringLit.toString)
    }
    
  def equalop: Parser[Leaf[String]] =
    ("==" | "!=") ^^
    {
      case stringLit => Leaf(stringLit.toString)
    }
    
  def relop: Parser[Leaf[String]] =
    (">=" | "<=" | ">" | "<") ^^
    {
      case stringLit => Leaf(stringLit.toString)
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
    "els(?!e$)|whil(?!e$)|i(?!f$)|[A-Za-z][0-9A-Za-z]*".r
  
  def intconst: Parser[String] =
    "[0-9][0-9]*".r
  
  
}