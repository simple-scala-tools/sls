import mill._
import mill.define.Sources
import scalalib._

import $ivy.`com.disneystreaming.smithy4s::smithy4s-mill-codegen-plugin::0.18.34`
import _root_.smithy4s.codegen.mill._

object sls extends ScalaModule {

  def scalaVersion = "3.7.0-RC4"
  def moduleDeps   = Seq(bspJsonRpc)
  def mainClass    = Some("org.scala.abusers.sls.MyServer")

  def ivyDeps = Agg(
    ivy"tech.neander::langoustine-app::0.0.22",
    ivy"com.lihaoyi::os-lib:0.11.4",
    ivy"co.fs2::fs2-io:3.12.0"
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

object bspJsonRpc extends ScalaModule with Smithy4sModule {

  def scalaVersion = "3.6.4"

  def scalacOptions = Seq(
    "-Wunused:all"
  )

  override def smithy4sInputDirs: Sources = T.sources {
    super.smithy4sInputDirs() ++ Seq(
      PathRef(os.pwd / "lib" / "smithy.jar")
    )
  }

  def ivyDeps = Agg(
    ivy"co.fs2::fs2-io:3.13.0-M2",
    ivy"com.disneystreaming.smithy4s::smithy4s-json::${_root_.smithy4s.codegen.BuildInfo.version}",
    ivy"tech.neander::jsonrpclib-smithy4s::0.0.7+13-b18708ff-SNAPSHOT",
  )

  override def smithy4sIvyDeps = Agg(
    ivy"com.disneystreaming.alloy:alloy-core:0.2.2"
  )
}
