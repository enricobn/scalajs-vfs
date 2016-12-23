package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualFileRun {

  def run(input: VFSInput, output: VFSOutput, args: String*) : Either[IOError, Unit]

}
