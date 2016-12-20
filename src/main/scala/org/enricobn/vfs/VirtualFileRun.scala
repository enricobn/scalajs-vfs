package org.enricobn.vfs

import IOError._

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualFileRun {

  def run(args: String*) : IOEff[Unit]

}
