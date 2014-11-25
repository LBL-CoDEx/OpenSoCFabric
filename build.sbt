// Name of Organization
organization := "gov.lbl.crd.ftg"

// Version number of project
version := "1.0"

// Project Name
name := "OpenSoC"

// Version of Scala being used
scalaVersion := "2.10.2"

// Chisel compiler location
addSbtPlugin("com.github.scct" % "sbt-scct" % "0.2")

// Version of Chisel being used
libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.2.20"

