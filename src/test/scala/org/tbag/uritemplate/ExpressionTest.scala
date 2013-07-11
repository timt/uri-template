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
    "Path-style parameters, semicolon-prefixed l3" in {
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
    "String expansion with value modifiers l4" in {
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
    "Label expansion, dot-prefixed l4" in {
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
    "Path-style parameters, semicolon-prefixed l4" in {
      "{;hello:5}".expand must be equalTo (";hello=Hello")
      "{;list}".expand must be equalTo (";list=red,green,blue")
      "{;list*}".expand must be equalTo (";list=red;list=green;list=blue")
      "{;keys}".expand must be equalTo (";keys=semi,%3B,dot,.,comma,%2C")
      "{;keys*}".expand must be equalTo (";semi=%3B;dot=.;comma=%2C")
    }
    "Form-style query, ampersand-separated" in {
      "{?var:3}".expand must be equalTo ("?var=val")
      "{?list}".expand must be equalTo ("?list=red,green,blue")
      "{?list*}".expand must be equalTo ("?list=red&list=green&list=blue")
      "{?keys}".expand must be equalTo ("?keys=semi,%3B,dot,.,comma,%2C")
      "{?keys*}".expand must be equalTo ("?semi=%3B&dot=.&comma=%2C")
    }

    "Form-style query continuation" in {
      "{&var:3}".expand must be equalTo ("&var=val")
      "{&list}".expand must be equalTo ("&list=red,green,blue")
      "{&list*}".expand must be equalTo ("&list=red&list=green&list=blue")
      "{&keys}".expand must be equalTo ("&keys=semi,%3B,dot,.,comma,%2C")
      "{&keys*}".expand must be equalTo ("&semi=%3B&dot=.&comma=%2C")
    }
  }
  "2.4.1.  Prefix Values examples" should {
    implicit val variables: Map[String, Any] = Map(
      "var" -> "value",
      "semi" -> ";"
    )
    "work" in {
      "{var}".expand must be equalTo ("value")
      "{var:20}".expand must be equalTo ("value")
      "{var:3".expand must be equalTo ("val")
      "{semi}".expand must be equalTo ("%3B")
      "{semi:2}".expand must be equalTo ("%3B")
    }
  }
  "2.4.2.  Composite Values examples" should {
    implicit val variables: Map[String, Any] = Map(
      "year" -> List("1965", "2000", "2012"),
      "dom" -> List("example", "com")
    )
    "work" in {
      "{?year*}".expand must be equalTo ("?year=1965&year=2000&year=2012")
      "{.dom*}".expand must be equalTo (".example.com")
    }
  }
  "3.  Expansion examples" should {
    implicit val variables: Map[String, Any] = Map(
      "count" -> List("one", "two", "three"),
      "dom" -> List("example", "com"),
      "dub" -> "me/too",
      "hello" -> "Hello World!",
      "half" -> "50%",
      "var" -> "value",
      "who" -> "fred",
      "base" -> "http://example.com/home/",
      "path" -> "/foo/bar",
      "list" -> List("red", "green", "blue"),
      "keys" -> Map("semi" -> ";", "dot" -> ".", "comma" -> ","),
      "v" -> "6",
      "x" -> "1024",
      "y" -> "768",
      "empty" -> "",
      "empty_keys" -> Map[String, Any](),
      "undef" -> null
    )
    "3.2.1.  Variable Expansion examples" in {
      "{count}".expand must be equalTo ("one,two,three")
      "{count*}".expand must be equalTo ("one,two,three")
      "{/count}".expand must be equalTo ("/one,two,three")
      "{/count*}".expand must be equalTo ("/one/two/three")
      "{;count}".expand must be equalTo (";count=one,two,three")
      "{;count*}".expand must be equalTo (";count=one;count=two;count=three")
      "{?count}".expand must be equalTo ("?count=one,two,three")
      "{?count*}".expand must be equalTo ("?count=one&count=two&count=three")
      "{&count*}".expand must be equalTo ("&count=one&count=two&count=three")
    }
    "3.2.2.  Simple String Expansion: {var} examples" in {
      "{var}".expand must be equalTo ("value")
      "{hello}".expand must be equalTo ("Hello%20World%21")
      "{half}".expand must be equalTo ("50%25")
      "{empty}".expand must be equalTo ("")
      "{undef}".expand must be equalTo ("")
      "{x,y}".expand must be equalTo ("1024,768")
      "{x,hello,y}".expand must be equalTo ("1024,Hello%20World%21,768")
      "{x,empty}".expand must be equalTo ("1024,")
      "{x,undef}".expand must be equalTo ("1024")
      "{undef,y}".expand must be equalTo ("768")
      "{var:3}".expand must be equalTo ("val")
      "{var:30}".expand must be equalTo ("value")
      "{list}".expand must be equalTo ("red,green,blue")
      "{list*}".expand must be equalTo ("red,green,blue")
      "{keys}".expand must be equalTo ("semi,%3B,dot,.,comma,%2C")
      "{keys*}".expand must be equalTo ("semi=%3B,dot=.,comma=%2C")
    }
    "3.2.3.  Reserved Expansion: {+var} examples" in {
      "{+var}".expand must be equalTo ("value")
      "{+hello}".expand must be equalTo ("Hello%20World!")
      "{+half}".expand must be equalTo ("50%25")
      "{base}".expand must be equalTo ("http%3A%2F%2Fexample.com%2Fhome%2F")
      "{+base}".expand must be equalTo ("http://example.com/home/")
      "{+empty}".expand must be equalTo ("")
      "{+undef}".expand must be equalTo ("")
      "{+path}".expand must be equalTo ("/foo/bar")
      "{+path}".expand must be equalTo ("/foo/bar")
      "{+x,hello,y}".expand must be equalTo ("1024,Hello%20World!,768")
      "{+path,x}".expand must be equalTo ("/foo/bar,1024")
      "{+path:6}".expand must be equalTo ("/foo/b")
      "{+list}".expand must be equalTo ("red,green,blue")
      "{+list*}".expand must be equalTo ("red,green,blue")
      "{+keys}".expand must be equalTo ("semi,;,dot,.,comma,,")
      "{+keys*}".expand must be equalTo ("semi=;,dot=.,comma=,")
    }
    "3.2.4.  Fragment Expansion: {#var} examples" in {
      "{#var}".expand must be equalTo ("#value")
      "{#hello}".expand must be equalTo ("#Hello%20World!")
      "{#half}".expand must be equalTo ("#50%25")
      "{#empty}".expand must be equalTo ("#")
      "{#undef}".expand must be equalTo ("")
      "{#x,hello,y}".expand must be equalTo ("#1024,Hello%20World!,768")
      "{#path,x}".expand must be equalTo ("#/foo/bar,1024")
      "{#path:6}".expand must be equalTo ("#/foo/b")
      "{#list}".expand must be equalTo ("#red,green,blue")
      "{#list*}".expand must be equalTo ("#red,green,blue")
      "{#keys}".expand must be equalTo ("#semi,;,dot,.,comma,,")
      "{#keys*}".expand must be equalTo ("#semi=;,dot=.,comma=,")


    }
    "3.2.5.  Label Expansion with Dot-Prefix: {.var} examples" in {
      "{.who}".expand must be equalTo (".fred")
      "{.who,who}".expand must be equalTo (".fred.fred")
      "{.half,who}".expand must be equalTo (".50%25.fred")
      "{.dom*}".expand must be equalTo (".example.com")
      "{.var}".expand must be equalTo (".value")
      "{.empty}".expand must be equalTo (".")
      "{.undef}".expand must be equalTo ("")
      "{.var:3}".expand must be equalTo (".val")
      "{.list}".expand must be equalTo (".red,green,blue")
      "{.list*}".expand must be equalTo (".red.green.blue")
      "{.keys}".expand must be equalTo (".semi,%3B,dot,.,comma,%2C")
      "{.keys*}".expand must be equalTo (".semi=%3B.dot=..comma=%2C")
      "{.empty_keys}".expand must be equalTo ("")
      "{.empty_keys*}".expand must be equalTo ("")
    }
    "3.2.6.  Path Segment Expansion: {/var} examples" in {
      "{/who}".expand must be equalTo ("/fred")
      "{/who,who}".expand must be equalTo ("/fred/fred")
      "{/half,who}".expand must be equalTo ("/50%25/fred")
      "{/who,dub}".expand must be equalTo ("/fred/me%2Ftoo")
      "{/var}".expand must be equalTo ("/value")
      "{/var,empty}".expand must be equalTo ("/value/")
      "{/var,undef}".expand must be equalTo ("/value")
      "{/var,x}".expand must be equalTo ("/value/1024")
      "{/var:1,var}".expand must be equalTo ("/v/value")
      "{/list}".expand must be equalTo ("/red,green,blue")
      "{/list*}".expand must be equalTo ("/red/green/blue")
      "{/list*,path:4}".expand must be equalTo ("/red/green/blue/%2Ffoo")
      "{/keys}".expand must be equalTo ("/semi,%3B,dot,.,comma,%2C")
      "{/keys*}".expand must be equalTo ("/semi=%3B/dot=./comma=%2C")
    }
    "3.2.7 Path-Style Parameter Expansion: {;var} examples" in {
      "{;who}".expand must be equalTo (";who=fred")
      "{;half}".expand must be equalTo (";half=50%25")
      "{;empty}".expand must be equalTo (";empty")
      "{;v,empty,who}".expand must be equalTo (";v=6;empty;who=fred")
      "{;v,bar,who}".expand must be equalTo (";v=6;who=fred")
      "{;x,y}".expand must be equalTo (";x=1024;y=768")
      "{;x,y,empty}".expand must be equalTo (";x=1024;y=768;empty")
      "{;x,y,undef}".expand must be equalTo (";x=1024;y=768")
      "{;hello:5}".expand must be equalTo (";hello=Hello")
      "{;list}".expand must be equalTo (";list=red,green,blue")
      "{;list*}".expand must be equalTo (";list=red;list=green;list=blue")
      "{;keys}".expand must be equalTo (";keys=semi,%3B,dot,.,comma,%2C")
      "{;keys*}".expand must be equalTo (";semi=%3B;dot=.;comma=%2C")
    }
    "3.2.8.  Form-Style Query Expansion: {?var}" in {
      "{?who}".expand must be equalTo ("?who=fred")
      "{?half}".expand must be equalTo ("?half=50%25")
      "{?x,y}".expand must be equalTo ("?x=1024&y=768")
      "{?x,y,empty}".expand must be equalTo ("?x=1024&y=768&empty=")
      "{?x,y,undef}".expand must be equalTo ("?x=1024&y=768")
      "{?var:3}".expand must be equalTo ("?var=val")
      "{?list}".expand must be equalTo ("?list=red,green,blue")
      "{?list*}".expand must be equalTo ("?list=red&list=green&list=blue")
      "{?keys}".expand must be equalTo ("?keys=semi,%3B,dot,.,comma,%2C")
      "{?keys*}".expand must be equalTo ("?semi=%3B&dot=.&comma=%2C")
    }
    "3.2.9.  Form-Style Query Continuation: {&var}" in {
      "{&who}".expand must be equalTo ("&who=fred")
      "{&half}".expand must be equalTo ("&half=50%25")
      "{&x}".expand must be equalTo ("&x=1024")
      "{&x,y,empty}".expand must be equalTo ("&x=1024&y=768&empty=")
      "{&x,y,undef}".expand must be equalTo ("&x=1024&y=768")
      "{&var:3}".expand must be equalTo ("&var=val")
      "{&list}".expand must be equalTo ("&list=red,green,blue")
      "{&list*}".expand must be equalTo ("&list=red&list=green&list=blue")
      "{&keys}".expand must be equalTo ("&keys=semi,%3B,dot,.,comma,%2C")
      "{&keys*}".expand must be equalTo ("&semi=%3B&dot=.&comma=%2C")
    }
  }

  implicit def toExpression(expression: String): Expression = Expression(expression)

}