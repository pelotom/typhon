name := "Typhon"

description := "A simple typed functional language which compiles to Java"

version := "0.1-SNAPSHOT"

homepage := Some(url("http://github.com/pelotom/typhon"))

organization := "org.pelotom"

startYear := Some(2013)

scalaVersion := "2.10.1"

libraryDependencies <++= (scalaVersion) { sv => Seq("org.scala-lang" % "scala-reflect" % sv) }

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint")

scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions", "-language:postfixOps")
