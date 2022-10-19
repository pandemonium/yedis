name         := "yedis"
version      := "0.1"
scalaVersion := "3.2.0"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "dev.zio"         %% "zio"             % "2.0.2",
  "dev.zio"         %% "zio-streams"     % "2.0.2",
)