package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualFolder extends VirtualNode {
  @throws[VirtualIOException]
  def folders: Set[VirtualFolder]

  @throws[VirtualIOException]
  def files: Set[VirtualFile]

  @throws[VirtualIOException]
  def mkdir(name: String): VirtualFolder

  @throws[VirtualIOException]
  def deleteFile(name: String)

  @throws[VirtualIOException]
  def deleteFolder(name: String)

  @throws[VirtualIOException]
  def touch(name: String): VirtualFile

  @throws[VirtualIOException]
  def createExecutableFile(name: String, run: VirtualFileRun): VirtualFile

  @throws[VirtualIOException]
  def createDynamicFile(name: String, content: () => AnyRef): VirtualFile

  @throws[VirtualIOException]
  def rename(name: String)

  @throws[VirtualIOException]
  def findFileOrThrow(fileName: String): VirtualFile = {
    findFileOrThrow(fileName, file => true)
  }

  @throws[VirtualIOException]
  def findFileOrThrow(fileName: String, predicate: Function[VirtualFile, Boolean]): VirtualFile = {
    val file: Option[VirtualFile] = findFile(fileName, predicate)
    if (file.isDefined) {
      file.get
    }
    else {
      throw new VirtualIOException(fileName + ": No such file")
    }
  }

  @throws[VirtualIOException]
  def findFile(fileName: String): Option[VirtualFile] = {
    findFile(fileName, file => true)
  }

  @throws[VirtualIOException]
  def findFile(fileName: String, predicate: Function[VirtualFile, Boolean]): Option[VirtualFile] = {
    files.find(file => file.name == fileName && predicate.apply(file))
  }

  @throws[VirtualIOException]
  def findFolderOrThrow(name: String, predicate: Function[VirtualFolder, Boolean]): VirtualFolder = {
    val folder: Option[VirtualFolder] = findFolder(name, predicate)
    if (folder.isDefined) {
      folder.get
    } else {
      throw new VirtualIOException(name + ": No such directory")
    }
  }

  @throws[VirtualIOException]
  def findFolder(name: String, predicate: Function[VirtualFolder, Boolean]): Option[VirtualFolder] = {
    folders.find(folder => folder.name == name && predicate.apply(folder))
  }

  @throws[VirtualIOException]
  def resolveFolder(path: String): VirtualFolder = {
    if (path.startsWith("/")) {
      return root.resolveFolder(path.substring(1))
    }
    else if (path.isEmpty) {
      return this
    }
    val elements: Array[String] = path.split("/")
    var result: VirtualFolder = this
    for (element <- elements) {
      if (element == "..") {
        if (result.parent != null) {
          result = result.parent
        }
      } else if (element != ".") {
        result = result.findFolderOrThrow(element, folder => true)
      }
    }
    result
  }

  def root: VirtualFolder = {
    if (parent == null) {
      return this
    }
    parent.root
  }
}
