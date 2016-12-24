package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
//@JSExportAll
trait VirtualFile extends VirtualNode {
  def content: Either[IOError, AnyRef]

  def content_=(content: AnyRef) : Either[IOError, Unit]

}
