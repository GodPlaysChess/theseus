name := "theseus"

version := "0.1"

scalaVersion := "2.12.6"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.2.7"

libraryDependencies += "commons-codec" % "commons-codec" % "1.11"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += Resolver.sonatypeRepo("releases")

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/releases"
//
//libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"
//
//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
//
//parallelExecution in Test := false
//
//libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2"
//
//logBuffered := false
