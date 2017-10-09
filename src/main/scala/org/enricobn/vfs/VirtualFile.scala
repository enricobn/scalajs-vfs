package org.enricobn.vfs

import IOError._

/**
  * Created by enrico on 12/2/16.
  */
//@JSExportAll
trait VirtualFile extends VirtualNode {

  def content: Either[IOError, AnyRef]

  def content_=(content: AnyRef) : Option[IOError]

  def contentAs[T](clazz: Class[T]) : Either[IOError, T] =
    content match {
      case Left(error) => Left(error)
      case Right(content) =>
        if (clazz.isAssignableFrom(content.getClass)) {
          Right(content.asInstanceOf[T])
        } else {
          s"Wrong type: ${content.getClass.getSimpleName}".ioErrorE
        }
    }

}
