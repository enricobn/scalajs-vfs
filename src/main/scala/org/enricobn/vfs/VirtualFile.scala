package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualFile extends VirtualNode {
  @throws[VirtualIOException]
  def getContent: AnyRef

  @throws[VirtualIOException]
  def setContent(content: AnyRef)

  @throws[VirtualIOException]
  def run(args: String*) {
    throw new VirtualIOException(getName + ": unsupported executable format")
  }
}
