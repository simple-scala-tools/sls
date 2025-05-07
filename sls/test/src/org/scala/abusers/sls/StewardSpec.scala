package org.scala.abusers.sls

import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.IO
import cats.syntax.all.*
import weaver.*

object StewardSpec extends SimpleIOSuite:

  test("steward should release as many times as it acquired") {
    (for
      acquired <- Ref.of[IO, Int](0)
      released <- Ref.of[IO, Int](0)
    yield (acquired, released))
      .flatMap { case (acquired, released) =>

        Steward[IO]
          .use { steward =>
            val justSomeResource = Resource.make(acquired.update(_ + 1))(_ => released.update(_ + 1))
            (1 to 10).toList.map(_ => justSomeResource).traverse(steward.acquire)
          }
          .flatMap { _ =>
            for
              acquiredCount <- acquired.get
              releasedCount <- released.get
            yield expect(acquiredCount == releasedCount)
          }
      }
  }
