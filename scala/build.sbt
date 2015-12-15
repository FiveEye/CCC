lazy val root = (project in file(".")).
  settings(
    name := "CCC",
    version := "0.1",
    scalaVersion := "2.11.6"
  )
  
libraryDependencies += "de.fosd.typechef" %% "frontend" % "0.3.7"


