import coursier.core.Repository
import coursier.maven.MavenRepository
import mill._
import mill.define.Sources
import scalalib._

import $ivy.`com.disneystreaming.smithy4s::smithy4s-mill-codegen-plugin::0.18.35`
import _root_.smithy4s.codegen.mill._

trait CommonScalaModule extends ScalaModule {
  override def repositoriesTask: Task[Seq[Repository]] = T.task {
    Seq(
      MavenRepository(
        "https://s01.oss.sonatype.org/content/repositories/snapshots"
      )
    ) ++ super.repositoriesTask()
  }

  def scalaVersion = "3.7.0"
}

object sls extends CommonScalaModule {

  def moduleDeps = Seq(bspJsonRpc)
  def mainClass  = Some("org.scala.abusers.sls.SimpleScalaServer")

  def ivyDeps = Agg(
    ivy"co.fs2::fs2-io:3.13.0-M2",
    ivy"tech.neander::jsonrpclib-fs2::0.0.7+18-0e7dd223-SNAPSHOT".forceVersion(),
    ivy"tech.neander::langoustine-app::0.0.22",
    ivy"com.lihaoyi::os-lib:0.11.4",
    ivy"org.polyvariant.smithy4s-bsp::bsp4s:0.1-d25359c-20250506T013346Z-SNAPSHOT",
  )

  def scalacOptions = Seq(
    "-Wunused:all"
  )

  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-cats:0.8.4"
    )
    def testFramework = "weaver.framework.CatsEffect"
  }
}

object bspJsonRpc extends CommonScalaModule with Smithy4sModule {

  def scalacOptions = Seq(
    "-Wunused:all"
  )

  def ivyDeps = Agg(
    ivy"com.disneystreaming.smithy4s::smithy4s-core::${_root_.smithy4s.codegen.BuildInfo.version}"
  )

  override def smithy4sIvyDeps = Agg(
    ivy"tech.neander:jsonrpclib-smithy:0.0.7+18-0e7dd223+20250506-0324-SNAPSHOT"
  )
}
