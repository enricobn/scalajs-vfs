package org.enricobn.vfs.inmemory

import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Created by enrico on 1/3/17.
  */
class InMemoryPermissionsSpec extends AnyFlatSpec with MockFactory with Matchers {

  "octal of rwx rwx rwx" should "be 777" in {
    val example = new InMemoryPermissions
    example.owner.execute = true
    example.group.execute = true
    example.others.execute = true
    example.others.write = true

    assert(example.owner.octal == 7)

    assert(example.group.octal == 7)

    assert(example.others.octal == 7)

    assert(example.octal == 777)
  }

}
