package org.scala.abusers.pc

import cats.effect.std.Mutex
import cats.effect.IO

import java.net.URLClassLoader
import java.util.ServiceLoader

class BlockingServiceLoader(mutex: Mutex[IO]):

  /*
   * ServiceLoader is not thread safe. That's why we are blocking it's execution.
   */
  def load[T](cls: Class[T], className: String, classloader: URLClassLoader): IO[T] =
    mutex.lock.surround:
        IO.blocking: // ServiceLoader loads classfiles thus is blocking
            val services = ServiceLoader.load(cls, classloader).iterator()
            if services.hasNext then services.next()
            else
              // NOTE(olafur): ServiceLoader doesn't find the service on Appveyor for
              // some reason, I'm unable to reproduce on my computer. Here below we
              // fallback to manual classloading.
              val cls  = classloader.loadClass(className)
              val ctor = cls.getDeclaredConstructor()
              ctor.setAccessible(true)
              ctor.newInstance().asInstanceOf[T]

object BlockingServiceLoader:
  private[pc] def instance: IO[BlockingServiceLoader] = Mutex[IO].map(BlockingServiceLoader.apply)
