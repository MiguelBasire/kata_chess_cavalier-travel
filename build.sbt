name := "chess-cavalier-travel"

version :=  "1.0"

scalaVersion := "2.13.5"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "2.5.3"

mainClass := Some("miguel.kata.cavalier.Main")