lazy val versions = new {
  lazy val specs2 = "3.8.6"
  lazy val cats = "0.8.1"
  lazy val enumeratum = "1.5.3"
}

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % versions.cats,
  "com.beachape" %% "enumeratum" % versions.enumeratum,
  "org.specs2" %% "specs2-core" % versions.specs2 % Test,
  "org.specs2" %% "specs2-scalacheck" % versions.specs2 % Test
)
