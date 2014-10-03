name := "hackerrank"

organization := "kissthinker"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:reflectiveCalls",
  "-language:postfixOps"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.2" % "test",
  "org.scalatest" %% "scalatest" % "2.1.6" % "test"
)