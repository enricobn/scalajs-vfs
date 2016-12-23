package org.enricobn.vfs

/**
  * Created by enrico on 12/13/16.
  */
trait VFSOutput {

  def write(s: String) : Unit

  def flush() : Unit

}
