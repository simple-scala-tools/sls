package org.scala.abusers.pc

import cats.effect.IO
import coursier.*
import coursier.cache.*

import java.io.File
import java.net.URLClassLoader
import scala.jdk.CollectionConverters.*
import scala.meta.pc.PresentationCompiler
import com.evolution.scache.Cache as SCache
import bsp.BuildTargetIdentifier
import com.evolution.scache.ExpiringCache
import scala.concurrent.duration.*
import java.nio.file.Path

class PresentationCompilerProvider(serviceLoader: BlockingServiceLoader, compilers: SCache[IO, ScalaVersion, PresentationCompiler]):
  import CoursiercatsInterop.*
  private val cache = FileCache[IO] // .withLogger TODO No completions here

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

  private def createPC(scalaVersion: ScalaVersion, projectClasspath: List[Path]) =
    for
      compilerClasspath <- fetchPresentationCompilerJars(scalaVersion)
      classloader       <- freshPresentationCompilerClassloader(Nil, compilerClasspath)
      pc <- serviceLoader.load(classOf[PresentationCompiler], PresentationCompilerProvider.classname, classloader)
    yield pc.newInstance("random", projectClasspath.asJava, Nil.asJava)

  def get(scalaVersion: ScalaVersion, projectClasspath: List[Path]): IO[PresentationCompiler] =
    compilers.getOrUpdate(scalaVersion)(createPC(scalaVersion, projectClasspath))

object PresentationCompilerProvider:
  val classname = "dotty.tools.pc.ScalaPresentationCompiler"

  def instance: IO[PresentationCompilerProvider] =
    for
      serviceLoader <- BlockingServiceLoader.instance
      pcProvider <- SCache.expiring[IO, ScalaVersion, PresentationCompiler]( // we will need to move this out because other services will want to manage the state of the cache and invalidate when configuration changes also this shoul be ModuleFingerprint or something like that
        ExpiringCache.Config(expireAfterRead = 5.minutes),
        None
      ).use(cache => IO(PresentationCompilerProvider(serviceLoader, cache)))
    yield pcProvider

opaque type ScalaVersion = String

extension (scalaVersion: ScalaVersion) def value: String = scalaVersion

object ScalaVersion:
  def apply(scalaVersion: String): ScalaVersion = scalaVersion
