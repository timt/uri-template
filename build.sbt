name := "URITemplate"

organization := "org.tbag"

version := "0.1"

scalaVersion := "2.10.2"

resolvers += "Dans repo" at "http://repo.bodar.com/"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" withSources(),
    "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test" withSources(),
    "org.springframework" % "spring-web" % "3.2.3.RELEASE" withSources(),
    "com.googlecode.utterlyidle" % "utterlyidle" % "652" withSources() intransitive()
)