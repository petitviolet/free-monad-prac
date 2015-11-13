name := "free_monad_db"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.scalikejdbc" %% "scalikejdbc-test" % "2.2.8" % "test",
  "org.mockito" % "mockito-core" % "1.10.19" % "test",
  "org.scalikejdbc" %% "scalikejdbc" % "2.2.+",
  "com.h2database" % "h2" % "1.4.+",
  "ch.qos.logback" % "logback-classic" % "1.1.+"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  //"-Ywarn-value-discard",
  "-Ywarn-unused-import"
)

//scalikejdbcSettings

javacOptions ++= Seq("-encoding", "UTF-8")
