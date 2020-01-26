package org.enricobn.vfs
import org.enricobn.vfs.utils.Utils.RightBiasedEither

class VirtualFileWithContent[T <: AnyRef](clazz: Class[T], val fs: VirtualFS, val path: VirtualPath)
                                         (implicit authentication: Authentication) {

  private def fileE = fs.root.resolveFileOrError(path)

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

  def setContent(content: T): Either[IOError, Unit] = {
    val parentFolderE = path.parentFragments match {
      case Some(parentPath) => fs.root.resolveFolderOrError(parentPath.path)
      case _ => Right(fs.root)
    }

    for {
      parentFolder <- parentFolderE
      foundFile <- parentFolder.findFile(path.name)
      file <- if (foundFile.isEmpty) parentFolder.touch(path.name) else fileE
      _ <- file.setContent(content)
    } yield ()
  }

}