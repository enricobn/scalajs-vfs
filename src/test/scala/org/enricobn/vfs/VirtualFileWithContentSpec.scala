package org.enricobn.vfs

import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

class VirtualFileWithContentSpec extends AnyFlatSpec with MockFactory with Matchers {

  "setContent" should "create a file if it does not exist" in {
    val fs = InMemoryFS("root").fold({ e => fail(e.message) }, { folder => folder })

    val helloWorld = for {
      authentication <- fs.vum.logUser("root", "root")
      path <- VirtualPath.absolute("home", "afile")
      sut = new VirtualFileWithContent(classOf[String], fs, path)(authentication)
      _ <- sut.setContent("Hello world")
      content <- sut.content()(authentication)
    } yield content

    helloWorld match {
      case Right(c) => assert(c == "Hello world")
      case Left(e) => fail(e.message)
    }

  }

}
