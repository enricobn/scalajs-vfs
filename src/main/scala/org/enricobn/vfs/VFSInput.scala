package org.enricobn.vfs

/**
  * Created by enrico on 12/13/16.
  */
trait VFSInput {
  def subscribe(fun: Function[String,Unit]) : Unit
}
