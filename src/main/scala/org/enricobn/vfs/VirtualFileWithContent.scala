package org.enricobn.vfs

import org.enricobn.vfs.utils.Utils.RightBiasedEither

class VirtualFileWithContent[T <: AnyRef](clazz: Class[T], val fs: VirtualFS, val path: AbsolutePath)
                                         (implicit authentication: Authentication) {

  def file: Either[IOError, VirtualFile] = path.toFile(fs)

  def mapContent(mapFunction: T => T): Either[IOError, Unit] =
    for {
      file <- file
      content <- file.contentAs(clazz)
      newContent = mapFunction.apply(content)
      _ <- file.setContent(newContent.asInstanceOf[AnyRef])
    } yield ()

  def content()(implicit authentication: Authentication): Either[IOError, T] =
    for {
      file <- file
      content <- file.getContent
    } yield content.asInstanceOf[T]

  def setContent(content: T): Either[IOError, Unit] =
    for {
      file <- path.toFileOrCreate(fs.root)
      _ <- file.setContent(content)
    } yield ()

}