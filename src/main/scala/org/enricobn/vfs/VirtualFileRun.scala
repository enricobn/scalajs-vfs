package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualFileRun {

  def run(args: String*) : Either[IOError, Unit]

}
