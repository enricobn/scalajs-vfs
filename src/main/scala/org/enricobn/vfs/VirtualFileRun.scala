package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualFileRun {
  @throws[VirtualIOException]
  def run(args: String*)

}
