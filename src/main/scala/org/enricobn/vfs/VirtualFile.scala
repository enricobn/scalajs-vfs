package org.enricobn.vfs

import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */
//@JSExportAll
trait VirtualFile extends VirtualNode {
  def content: Either[IOError, AnyRef]

  def content_=(content: AnyRef) : IOEff[Unit]

  def run(args: String*) : IOEff[Unit] =
    (name + ": unsupported executable format").ioErrorE

}
