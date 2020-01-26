package org.enricobn.vfs
import org.enricobn.vfs.utils.Utils.RightBiasedEither

class VirtualFileWithContent[T <: AnyRef](clazz: Class[T], val fs: VirtualFS, val path: VirtualPath)
                                         (implicit authentication: Authentication) {

  private val fileE = fs.root.resolveFileOrError(path)

  def mapContent(mapFunction: T => T): Either[IOError, Unit] =
    for {
      file <- fileE
      content <- file.contentAs(clazz)
      newContent = mapFunction.apply(content)
      _ <- file.setContent(newContent.asInstanceOf[AnyRef])
    } yield ()

  def content()(implicit authentication: Authentication): Either[IOError, T] =
    for {
      file <- fileE
      content <- file.getContent
    } yield content.asInstanceOf[T]

  def setContent(content: T): Either[IOError, T] =
    for {
      file <- fileE
      content <- file.setContent(content)
    } yield content.asInstanceOf[T]

}