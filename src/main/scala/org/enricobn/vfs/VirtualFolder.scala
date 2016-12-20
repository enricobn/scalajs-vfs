package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualFolder extends VirtualNode {

  def folders: Either[IOError, Set[VirtualFolder]]

  def files: Either[IOError, Set[VirtualFile]]

  def mkdir(name: String): Either[IOError, VirtualFolder]

  def deleteFile(name: String) : Either[IOError, Boolean]

  def deleteFolder(name: String) : Either[IOError, Boolean]

  def touch(name: String): Either[IOError, VirtualFile]

  def createExecutableFile(name: String, run: VirtualFileRun): Either[IOError, VirtualFile]

  def createDynamicFile(name: String, content: () => AnyRef): Either[IOError, VirtualFile]

  def rename(name: String) : Either[IOError, Unit]

  def findFile(fileName: String): Either[IOError, Option[VirtualFile]] = {
    findFile(fileName, _ => true)
  }

  def findFile(fileName: String, predicate: Function[VirtualFile, Boolean]): Either[IOError, Option[VirtualFile]] = {
    files.right.map(_.find(file => file.name == fileName && predicate.apply(file)))
  }

  def findFolder(name: String, predicate: Function[VirtualFolder, Boolean]): Either[IOError, Option[VirtualFolder]] = {
    folders.right.map(_.find(folder => folder.name == name && predicate.apply(folder)))
  }

  def resolveFolder(path: String): Either[IOError, Option[VirtualFolder]] = {
    if (path.startsWith("/")) {
      return root.resolveFolder(path.substring(1))
    }
    else if (path.isEmpty) {
      return Right(Some(this))
    }
    val elements: Array[String] = path.split("/")
    var result = this
    for (element <- elements) {
      if (element == "..") {
        if (result.parent != null) {
          result = result.parent
        }
      } else if (element != ".") {
        result.findFolder(element, folder => true) match {
          case Left(error) => return Left(error)
          case Right(None) => return Right(None)
          case Right(Some(f)) => result = f
        }
      }
    }
    Right(Some(result))
  }

  def root: VirtualFolder = {
    if (parent == null) {
      this
    } else {
      parent.root
    }
  }
}
