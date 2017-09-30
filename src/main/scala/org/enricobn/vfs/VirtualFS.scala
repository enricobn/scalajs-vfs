package org.enricobn.vfs

/**
  * Created by enrico on 12/3/16.
  */

object VirtualFS {

  def root: String = "/"

  def pathSeparator: String = "/"

  def selfFragment: String = "."

  def parentFragment: String = ".."

  def rootPath: VirtualPath = VirtualPath(List(RootFragment()))

}

trait VirtualFS {

  def root: VirtualFolder

}
