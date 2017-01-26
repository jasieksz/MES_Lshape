name := "rrir-project"
version := "0.1"

scalaVersion := "2.12.1"

resolvers += "jzy3d-snapshots" at "http://maven.jzy3d.org/snapshots"
resolvers += "jzy3d-releases" at "http://maven.jzy3d.org/releases"

libraryDependencies += "org.jzy3d" % "jzy3d-api" % "0.9.1"
libraryDependencies += "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.25"


