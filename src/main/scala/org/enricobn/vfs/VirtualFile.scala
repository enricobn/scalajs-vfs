package org.enricobn.vfs

import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */
//@JSExportAll
trait VirtualFile extends VirtualNode {

  def getContent(implicit authentication: Authentication) : Either[IOError, AnyRef]

  def setContent(content: AnyRef)(implicit authentication: Authentication): Option[IOError]

  def contentAs[T](clazz: Class[T])(implicit authentication: Authentication): Either[IOError, T] =
    getContent(authentication) match {
      case Left(error) => Left(error)
      case Right(content) =>
        if (clazz.isAssignableFrom(content.getClass)) {
          Right(content.asInstanceOf[T])
        } else {
          s"Wrong type: ${content.getClass.getSimpleName}".ioErrorE
        }
    }

}
