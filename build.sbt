name := "URITemplate"

organization := "org.tbag"

version := "0.2"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" withSources(),
    "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test" withSources(),
    "org.springframework" % "spring-web" % "3.2.3.RELEASE" withSources()
)

publishTo <<= (version) { version: String =>
  val github = "/Users/timt/Projects/timt.github.com/repo/"
  if (version.trim.endsWith("SNAPSHOT")) Some(Resolver.file("file",  new File( github + "snapshots/")))
  else                                   Some(Resolver.file("file",  new File( github + "releases/")))
}
