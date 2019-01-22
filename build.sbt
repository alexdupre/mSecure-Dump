name := "mSecure-Dump"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "commons-io"             % "commons-io"    % "2.6",
  "commons-codec"          % "commons-codec" % "1.11",
  "org.scala-lang.modules" %% "scala-xml"    % "1.1.1",
  "com.github.tototoshi"   %% "scala-csv"    % "1.3.5"
)
