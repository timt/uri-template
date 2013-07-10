package org.tbag.uritemplate

import org.specs2.mutable.Specification


class ExpressionTest extends Specification {
  "Expression(expression)" should {
    "represent an expression" in {
      Expression("?query,number").expression must be equalTo ("?query,number")
    }
    "strip leading and trailing curly braces from an expression" in {
      Expression("{?query,number}").expression must be equalTo ("?query,number")
    }
  }

  "Level 1 examples" should {
    implicit val variables = Map(
      "var" -> "value",
      "hello" -> "Hello World!")
    "{var} => 'value'" in {
      "{var}".expand must be equalTo ("value")
    }
    "{hello} => 'Hello%20World%21'" in {
      "{hello}".expand must be equalTo ("Hello%20World%21")
    }
  }

  "Level 2 examples" should {
    implicit val variables = Map(
      "var" -> "value",
      "hello" -> "Hello World!",
      "path" -> "/foo/bar"
    )
    "+ Reserved string expansion" in {
      "{+var}".expand must be equalTo ("value")
      "{+hello}".expand must be equalTo ("Hello%20World!")
      "{+path}".expand must be equalTo ("/foo/bar")
    }
    "# Fragment expansion, crosshatch-prefixed " in {
      "{#var}".expand must be equalTo ("#value")
      "{#hello}".expand must be equalTo ("#Hello%20World!")
    }
  }

  "Level 3 examples" should {
    implicit val variables = Map(
      "var" -> "value",
      "hello" -> "Hello World!",
      "empty" -> "",
      "path" -> "/foo/bar",
      "x" -> "1024",
      "y" -> "768"
    )
    "String expansion with multiple variables" in {
      "{x,y}".expand must be equalTo ("1024,768")
      "{x,hello,y}".expand must be equalTo ("1024,Hello%20World%21,768")
    }
    "Reserved expansion with multiple variables" in {
      "{+x,hello,y}".expand must be equalTo ("1024,Hello%20World!,768")
      "{+path,x}".expand must be equalTo ("/foo/bar,1024")
    }
    "Fragment expansion with multiple variables" in {
      "{#x,hello,y}".expand must be equalTo ("#1024,Hello%20World!,768")
      "{#path,x}".expand must be equalTo ("#/foo/bar,1024")
    }
    "Label expansion, dot-prefixed" in {
      "{.var}".expand must be equalTo (".value")
      "{.x,y}".expand must be equalTo (".1024.768")
    }
    "Path segments, slash-prefixed" in {
      "{/var}".expand must be equalTo ("/value")
      "{/var,x}".expand must be equalTo ("/value/1024")
    }
    "Path-style parameters, semicolon-prefixed" in {
      "{;x,y}".expand must be equalTo (";x=1024;y=768")
      "{;x,y,empty}".expand must be equalTo (";x=1024;y=768;empty")
    }
    "Form-style query, ampersand-separated " in {
      "{?x,y}".expand must be equalTo ("?x=1024&y=768")
      "{?x,y,empty}".expand must be equalTo ("?x=1024&y=768&empty=")
    }
    "Form-style query continuation" in {
      "{&x}".expand must be equalTo ("&x=1024")
      "{&x,y,empty}".expand must be equalTo ("&x=1024&y=768&empty=")
    }
  }

  "Level 4 examples" should {
    implicit val variables: Map[String, Any] = Map(
      "var" -> "value",
      "hello" -> "Hello World!",
      "path" -> "/foo/bar",
      "list" -> List("red", "green", "blue"),
      "keys" -> Map("semi" -> ";", "dot" -> ".", "comma" -> ",")
    )
    "String expansion with value modifiers" in {
      "{var:3}".expand must be equalTo ("val")
      "{var:30}".expand must be equalTo ("value")
      "{list}".expand must be equalTo ("red,green,blue")
      "{list*}".expand must be equalTo ("red,green,blue")
      "{keys}".expand must be equalTo ("semi,%3B,dot,.,comma,%2C")
      "{keys*}".expand must be equalTo ("semi=%3B,dot=.,comma=%2C")
    }
    "Reserved expansion with value modifiers" in {
      "{+path:6}".expand must be equalTo ("/foo/b")
      "{+list}".expand must be equalTo ("red,green,blue")
      "{+list*}".expand must be equalTo ("red,green,blue")
      "{+keys}".expand must be equalTo ("semi,;,dot,.,comma,,")
      "{+keys*}".expand must be equalTo ("semi=;,dot=.,comma=,")
    }
    "Fragment expansion with value modifiers" in {
      "{#path:6}".expand must be equalTo ("#/foo/b")
      "{#list}".expand must be equalTo ("#red,green,blue")
      "{#list*}".expand must be equalTo ("#red,green,blue")
      "{#keys}".expand must be equalTo ("#semi,;,dot,.,comma,,")
      "{#keys*}".expand must be equalTo ("#semi=;,dot=.,comma=,")
    }
    "Label expansion, dot-prefixed" in {
      "{.var:3}".expand must be equalTo (".val")
      "{.list}".expand must be equalTo (".red,green,blue")
      "{.list*}".expand must be equalTo (".red.green.blue")
      "{.keys}".expand must be equalTo (".semi,%3B,dot,.,comma,%2C")
      "{.keys*}".expand must be equalTo (".semi=%3B.dot=..comma=%2C")
    }
    "Path segments, slash-prefixed" in {
      "{/var:1,var}".expand must be equalTo ("/v/value")
      "{/list}".expand must be equalTo ("/red,green,blue")
      "{/list*}".expand must be equalTo ("/red/green/blue")
      "{/list*,path:4}".expand must be equalTo ("/red/green/blue/%2Ffoo")
      "{/keys}".expand must be equalTo ("/semi,%3B,dot,.,comma,%2C")
      "{/keys*}".expand must be equalTo ("/semi=%3B/dot=./comma=%2C")
    }
    //    "Path-style parameters, semicolon-prefixed" in {
    //      "{;hello:5}".expand must be equalTo (";hello=Hello")
    //      "{;list}".expand must be equalTo (";list=red,green,blue")
    //      "{;list*}".expand must be equalTo (";list=red;list=green;list=blue")
    //      "{;keys}".expand must be equalTo (";keys=semi,%3B,dot,.,comma,%2C")
    //      "{;keys*}".expand must be equalTo (";semi=%3B;dot=.;comma=%2C")
    //    }
  }


  implicit def toExpression(expression: String): Expression = Expression(expression)

}