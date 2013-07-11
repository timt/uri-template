**uri-template** is a scala implementation of [RFC 6570](http://tools.ietf.org/html/rfc6570) - URI Template

Requirements
------------

* [scala](http://www.scala-lang.org) 2.10.2

Installation
------------

**sbt 0.12.3:**

Add the following lines to your build.sbt

    resolvers += "Tim Tennant's repo" at "http://timt.github.com/repo/releases/"

    addSbtPlugin("org.tbag" % "uri-template" % "0.1")

Cookbook
--------
    val uriTemplate = URITemplate("http://www.example.com/foo{?query,number}")
    uriTemplate.expand(
        "query" -> "mycelium",
        "number" -> 100
    )

Expands to http://www.example.com/foo?query=mycelium&number=100")

License
-------

Licensed under the New BSD License. See the LICENSE file for details.
