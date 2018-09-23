name := "theseus"

version := "0.1"

scalaVersion := "2.12.6"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.2.7"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.16.0"

libraryDependencies += "commons-codec" % "commons-codec" % "1.11"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += Resolver.sonatypeRepo("releases")
