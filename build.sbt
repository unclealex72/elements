name := """elements"""

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "com.beachape" %% "enumeratum" % "1.5.6",
  "org.specs2" %% "specs2-core" % "3.8.7" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")


fork in run := true
