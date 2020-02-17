name := "billing"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.2"

libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.1"

mainClass in (Compile, run) := Some("com.tesobe.obp.billing.Main")

mainClass in (Compile, packageBin) := Some("com.tesobe.obp.billing.Main")
