name := "Value-at-Risk: Chapter 04"

version := "1.0"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scalanlp" % "breeze_2.11" % "0.12",
  "org.scalafx" %% "scalafx" % "8.0.181-R13"
)

fork in run := true
