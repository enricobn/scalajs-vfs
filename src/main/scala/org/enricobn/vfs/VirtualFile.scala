package org.enricobn.vfs

import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */
//@JSExportAll
trait VirtualFile extends VirtualNode {
  def content: Either[IOError, AnyRef]

  def content_=(content: AnyRef) : Either[IOError, Unit]

  def run(args: String*) : Either[IOError, Unit] =
    (name + ": unsupported executable format").ioErrorE

}
