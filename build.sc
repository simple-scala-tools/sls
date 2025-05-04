import mill._
import mill.define.Sources
import scalalib._

import $ivy.`com.disneystreaming.smithy4s::smithy4s-mill-codegen-plugin::0.17.4`
import _root_.smithy4s.codegen.mill._

trait SharedJarClasspath extends ScalaModule {

  override def unmanagedClasspath = T {
    os.list(os.pwd / "lib").filter(_.ext == "jar").map(PathRef(_))
  }
}

object sls extends ScalaModule with SharedJarClasspath {

  def scalaVersion = "3.6.4"
  def moduleDeps   = Seq(bspJsonRpc)

  def ivyDeps = Agg(
    ivy"tech.neander::langoustine-app::0.0.21",
    ivy"com.lihaoyi::os-lib:0.11.4",
    ivy"co.fs2::fs2-io:3.12.0",
  )

  def scalacOptions = Seq(
    "-Wunused:all"
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-cats:0.8.4"
    )
    def testFramework = "weaver.framework.CatsEffect"
  }
}

object bspJsonRpc extends ScalaModule with Smithy4sModule with SharedJarClasspath {

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
    ivy"co.fs2::fs2-io:3.12.0",
    ivy"com.disneystreaming.smithy4s::smithy4s-json::${_root_.smithy4s.codegen.BuildInfo.version}",
  )

  override def smithy4sIvyDeps = Agg(
    ivy"com.disneystreaming.alloy:alloy-core:0.2.2"
  )
}
