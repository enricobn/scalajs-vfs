package org.enricobn.vfs

/**
  * Created by enrico on 12/3/16.
  */
trait VirtualFS {

  def root: VirtualFolder

  def rootPath: VirtualPath

  def pathSeparator: String

  def selfFragment: String

  def parentFragment: String

}
