package org.scala.abusers.pc

import cats.effect.IO
import coursier.*
import coursier.cache.*

import java.io.File
import java.net.URLClassLoader
import scala.jdk.CollectionConverters.*
import scala.meta.pc.PresentationCompiler

class PresentationCompilerProvider(serviceLoader: BlockingServiceLoader):
  import CoursiercatsInterop.*
  private val cache = FileCache[IO] // .withLogger TODO No completions here
  // There should be com.evolution.Scache I can explain why later

  private def fetchPresentationCompilerJars(scalaVersion: ScalaVersion): IO[Seq[File]] =
    val dep = Dependency(
      Module(Organization("org.scala-lang"), ModuleName("scala3-presentation-compiler_3")),
      scalaVersion.value,
    )

    Fetch(cache)
      .addDependencies(dep)
      .addRepositories( /* load from user config */ )
      .io

  private def freshPresentationCompilerClassloader(
      projectClasspath: Seq[File],
      compilerClasspath: Seq[File],
  ): IO[URLClassLoader] =
    IO.blocking:
        val fullClasspath    = compilerClasspath ++ projectClasspath
        val urlFullClasspath = fullClasspath.map(_.toURL)
        URLClassLoader(urlFullClasspath.toArray)

  def get(scalaVersion: ScalaVersion) =
    for
      compilerClasspath <- fetchPresentationCompilerJars(scalaVersion)
      classloader       <- freshPresentationCompilerClassloader(Nil, compilerClasspath)
      pc <- serviceLoader.load(classOf[PresentationCompiler], PresentationCompilerProvider.classname, classloader)
    yield pc.newInstance("random", compilerClasspath.map(_.toPath).asJava, Nil.asJava)

object PresentationCompilerProvider:
  val classname = "dotty.tools.pc.ScalaPresentationCompiler"

  def instance: IO[PresentationCompilerProvider] =
    BlockingServiceLoader.instance.map(PresentationCompilerProvider.apply)

opaque type ScalaVersion = String

extension (scalaVersion: ScalaVersion) def value: String = scalaVersion

object ScalaVersion:
  def apply(scalaVersion: String): ScalaVersion = scalaVersion
