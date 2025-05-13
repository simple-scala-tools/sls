package org.scala.abusers.pc

import bsp.BuildTargetIdentifier
import cats.effect.IO
import com.evolution.scache.Cache as SCache
import com.evolution.scache.ExpiringCache
import coursier.*
import coursier.cache.*
import org.scala.abusers.sls.ScalaBuildTargetInformation
import org.scala.abusers.sls.ScalaBuildTargetInformation.*
import os.Path

import java.net.URLClassLoader
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.meta.pc.PresentationCompiler

class PresentationCompilerProvider(
    serviceLoader: BlockingServiceLoader,
    compilers: SCache[IO, BuildTargetIdentifier, PresentationCompiler],
):
  import CoursiercatsInterop.*
  private val cache = FileCache[IO] // .withLogger TODO No completions here

  private def fetchPresentationCompilerJars(scalaVersion: ScalaVersion): IO[Seq[os.Path]] =
    val dep = Dependency(
      Module(Organization("org.scala-lang"), ModuleName("scala3-presentation-compiler_3")),
      scalaVersion.value,
    )

    Fetch(cache)
      .addDependencies(dep)
      .addRepositories( /* load from user config */ )
      .io
      .map(_.map(os.Path(_)))

  private def freshPresentationCompilerClassloader(
      projectClasspath: Seq[os.Path],
      compilerClasspath: Seq[os.Path],
  ): IO[URLClassLoader] =
    IO.blocking:
        val fullClasspath    = compilerClasspath ++ projectClasspath
        val urlFullClasspath = fullClasspath.map(_.toIO.toURL)
        URLClassLoader(urlFullClasspath.toArray)

  private def createPC(scalaVersion: ScalaVersion, projectClasspath: List[Path], scalacOptions: List[String]) =
    for
      compilerClasspath <- fetchPresentationCompilerJars(scalaVersion)
      classloader       <- freshPresentationCompilerClassloader(Nil, compilerClasspath)
      pc <- serviceLoader.load(classOf[PresentationCompiler], PresentationCompilerProvider.classname, classloader)
    yield pc.newInstance("random", projectClasspath.map(_.toNIO).asJava, scalacOptions.asJava)

  def get(info: ScalaBuildTargetInformation): IO[PresentationCompiler] =
    compilers.getOrUpdate(info.buildTarget.id)(createPC(info.scalaVersion, info.classpath, info.compilerOptions))

object PresentationCompilerProvider:
  val classname = "dotty.tools.pc.ScalaPresentationCompiler"

  def instance: IO[PresentationCompilerProvider] =
    for
      serviceLoader <- BlockingServiceLoader.instance
      pcProvider <- SCache
        .expiring[IO, BuildTargetIdentifier, PresentationCompiler]( // we will need to move this out because other services will want to manage the state of the cache and invalidate when configuration changes also this shoul be ModuleFingerprint or something like that
          ExpiringCache.Config(expireAfterRead = 5.minutes),
          None,
        )
        .use(cache => IO(PresentationCompilerProvider(serviceLoader, cache)))
    yield pcProvider

opaque type ScalaVersion = String

extension (scalaVersion: ScalaVersion) def value: String = scalaVersion

object ScalaVersion:
  def apply(scalaVersion: String): ScalaVersion = scalaVersion
