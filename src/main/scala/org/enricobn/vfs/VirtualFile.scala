package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualFile extends VirtualNode {
  @throws[VirtualIOException]
  def content: AnyRef

  @throws[VirtualIOException]
  def content_=(content: AnyRef)

  @throws[VirtualIOException]
  def run(args: String*) {
    throw new VirtualIOException(name + ": unsupported executable format")
  }
}
