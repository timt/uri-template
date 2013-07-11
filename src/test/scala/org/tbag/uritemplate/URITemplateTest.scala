package org.tbag.uritemplate

import org.specs2.mutable.Specification
import org.springframework.web.util.{UriTemplate => SuriTemplate}


class URITemplateTest extends Specification {

  "expanding the template http://www.example.com/foo{?query,number}" should {
    val uriTemplate = URITemplate("http://www.example.com/foo{?query,number}")
    "will populate the parameters" in {
      uriTemplate.expand(
        "query" -> "mycelium",
        "number" -> 100
      ) must be equalTo ("http://www.example.com/foo?query=mycelium&number=100")
    }
    "if 'query' is undefined will populate only the number parameter" in {
      uriTemplate.expand(
        "number" -> 100
      ) must be equalTo ("http://www.example.com/foo?number=100")
    }
    "if both variables are undefined will not populate query params" in {
      uriTemplate.expand() must be equalTo ("http://www.example.com/foo")
    }
  }
  "Some more examples from the RFC spec" should {
    "work" in {
      URITemplate("up{+path}{var}/here").expand(
        "var" -> "value",
        "path" -> "/foo/bar"
      ) must be equalTo ("up/foo/barvalue/here")
    }
  }
}