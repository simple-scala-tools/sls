import mill._
import scalalib._

object sls extends ScalaModule {
  def scalaVersion = "3.6.4"

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
