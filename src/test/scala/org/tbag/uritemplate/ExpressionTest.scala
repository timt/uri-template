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
    val variables = Map(
      "var" -> "value",
      "hello" -> "Hello World!")
    "{var} => 'value'" in {
      "{var}".expand(variables) must be equalTo ("value")
    }
    "{hello} => 'Hello%20World%21'" in {
      "{hello}".expand(variables) must be equalTo ("Hello%20World%21")
    }
  }

  "Level 2 examples" should {
    val variables = Map(
      "var" -> "value",
      "hello" -> "Hello World!",
      "path" -> "/foo/bar"
    )
    "+ Reserved string expansion" in {
      "{+var}".expand(variables) must be equalTo ("value")
      "{+hello}".expand(variables) must be equalTo ("Hello%20World!")
      "{+path}".expand(variables) must be equalTo ("/foo/bar")
    }
    "# Fragment expansion, crosshatch-prefixed " in {
      "{#var}".expand(variables) must be equalTo ("#value")
      "{#hello}".expand(variables) must be equalTo ("#Hello%20World!")
    }
  }

  "Level 3 examples" should {
    val variables = Map(
      "var" -> "value",
      "hello" -> "Hello World!",
      "empty" -> "",
      "path" -> "/foo/bar",
      "x" -> "1024",
      "y" -> "768"
    )
    "String expansion with multiple variables" in {
      "{x,y}".expand(variables) must be equalTo ("1024,768")
      "{x,hello,y}".expand(variables) must be equalTo ("1024,Hello%20World%21,768")
    }
    "Reserved expansion with multiple variables" in {
      "{+x,hello,y}".expand(variables) must be equalTo ("1024,Hello%20World!,768")
      "{+path,x}".expand(variables) must be equalTo ("/foo/bar,1024")
    }
    "Fragment expansion with multiple variables" in {
      "{#x,hello,y}".expand(variables) must be equalTo ("#1024,Hello%20World!,768")
      "{#path,x}".expand(variables) must be equalTo ("#/foo/bar,1024")
    }
    "Label expansion, dot-prefixed" in {
//      "{.var}".expand(variables) must be equalTo ("value")
      //        |     |    X{.x,y}               X.1024.768
    }

  }

  implicit def toExpression(expression: String): Expression = Expression(expression)

}