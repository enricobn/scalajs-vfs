package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
class VirtualSecurityException(message: String, cause: Throwable) extends Exception(message, cause) {

  def this(message: String) = this(message, null)

}
