name := "ocr"

version := "0.0.2-SNAPSHOT"

scalaVersion := "2.11.2"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" %% "breeze" % "0.11.2",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" %% "breeze-natives" % "0.11.2",
            "junit" % "junit" % "4.8.1" % "test",
            "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
            "com.sksamuel.scrimage" %% "scrimage-core" % "1.4.2",
            "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.2",
            "com.sksamuel.scrimage" %% "scrimage-filters" % "1.4.2",
            "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
            "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
)

libraryDependencies ++= Seq(
  "commons-io"                      % "commons-io"            % "2.4"
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.8-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

mainClass in (Compile, run) := Some("Main")