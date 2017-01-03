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

  def isRoot = path == "/"

  def parent : Option[VirtualAbsolutePath] =
    if (isRoot) {
      None
    } else {
      val lastSlash = path.lastIndexOf('/')
      if (lastSlash < 0) {
        None
      } else if (lastSlash == 0) {
        Some(ROOT)
      } else {
        Some(VirtualAbsolutePath(path.substring(0, lastSlash)))
      }
    }

}
