package org.enricobn.vfs.impl

/**
  * Created by enrico on 1/3/17.
  */
object VirtualAbsolutePath {
  val ROOT = new VirtualAbsolutePath("/")
}

case class VirtualAbsolutePath(path: String) {
  import VirtualAbsolutePath._

  require(path.startsWith("/"), "Not an absolute path.")

  private lazy val lastSlash = {
    val last = path.lastIndexOf('/')
    if (last == path.length - 1) {
      if (last == 0) {
        0
      } else {
        path.substring(0, last).lastIndexOf('/')
      }
    } else {
      last
    }
  }

  lazy val isRoot = path == "/"

  lazy val parent : Option[VirtualAbsolutePath] =
    if (isRoot) {
      None
    } else if (lastSlash == 0) {
      Some(ROOT)
    } else {
      Some(VirtualAbsolutePath(path.substring(0, lastSlash)))
    }

  lazy val name : String =
    if (isRoot) {
      "/"
    } else {
      if (path.endsWith("/")) {
        path.substring(lastSlash + 1, path.length - lastSlash - 1)
      } else {
        path.substring(lastSlash + 1)
      }
    }

  override def toString: String = path
  
}
