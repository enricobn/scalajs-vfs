package org.enricobn.vfs

/**
  * Created by enrico on 12/3/16.
  */

object VirtualFS {

  def root: String = "/"

  def pathSeparator: String = "/"

  def selfFragment: String = "."

  def parentFragment: String = ".."

}

trait VirtualFS {

  def root: VirtualFolder

  val notifier: VirtualFSNotifier

}
