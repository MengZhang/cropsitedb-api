name := "cropsitedb-api"

version := "2.0.9"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.postgresql"   % "postgresql"          % "9.3-1102-jdbc41",
  "org.mariadb.jdbc" % "mariadb-java-client" % "1.1.8",
  "org.agmip.ace"    % "ace-core"            % "2.1.0",
  "org.apache.tika"  % "tika-core"           % "1.6",
  "org.agmip"        % "dome"                % "1.4.12",
  "org.agmip"        % "acmo"                % "1.1.8",
  "org.agmip.tools"  %% "data-seam"          % "0.1.0"
)

doc in Compile <<= target.map(_ / "none")

mappings in Universal := {
	val orig = (mappings in Universal).value
	orig.filterNot { case (_, file) => file.endsWith("application.conf") || file.endsWith(".DS_Store")  }
}
