package org.enricobn.vfs

import org.enricobn.vfs.IOError._
import org.enricobn.vfs.utils.Utils
import org.enricobn.vfs.utils.Utils.RightBiasedEither

/**
  * Created by enrico on 5/9/17.
  */

object VirtualPath {

  val root = AbsolutePath(RelativePath(Seq.empty))

  def of(node: VirtualNode): Either[IOError, AbsolutePath] =
    of(node.path).map(_.asInstanceOf[AbsolutePath])

  def of(path: String): Either[IOError, VirtualPath] = {
    if (path == VirtualFS.pathSeparator)
      return Right(root)

    val absolute = path.startsWith(VirtualFS.pathSeparator)

    val relativePath =
      if (absolute)
        path.substring(1)
      else
        path

    val fragments = relativePath.split(VirtualFS.pathSeparator).toList

    if (absolute) {
      VirtualPath.relative(fragments: _*).right.map(p => AbsolutePath(p))
    } else {
      VirtualPath.relative(fragments: _*)
    }
  }

  def relative(fragments: String*): Either[IOError, RelativePath] = {
    for {
      fr <- Utils.lift(fragments.map(PathFragment.of))
      path <- Right(RelativePath(fr))
    } yield path
  }

  def absolute(fragments: String*): Either[IOError, AbsolutePath] = {
    relative(fragments:_*).right.map(path => AbsolutePath(path))
  }

  def absolute(from: VirtualFolder, path: VirtualPath): Either[IOError, AbsolutePath] =
    path match {
      case p: AbsolutePath => Right(p)
      case p: RelativePath =>
        for {
          vp <- VirtualPath.of(from)
          resolved = vp.andThen(p)
        } yield resolved
    }

}

sealed trait VirtualPath {

  def name: String

  def parent: Option[VirtualPath]

  def parentOrError : Either[IOError, VirtualPath] =
    parent match {
      case Some(x) => Right(x)
      case None => s"Cannot find parent of $this".ioErrorE
    }

  def toParentFolder(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFolder]

  def toFolder(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFolder]

  def toFile(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFile]

  def andThen(fragments: String*) : Either[IOError, VirtualPath]

  def andThen(fragment: PathFragment): VirtualPath

  def andThen(path: RelativePath): VirtualPath

  def toFileOrCreate(from: VirtualFolder)(implicit authentication: Authentication): Either[IOError, VirtualFile] = {
    val parentFolderE = toParentFolder(from)

    for {
      parentFolder <- parentFolderE
      foundFile <- parentFolder.findFile(name)
      file <- if (foundFile.isEmpty) parentFolder.touch(name) else Right(foundFile.get)
    } yield file
  }

  def toFolderOrCreate(from: VirtualFolder)(implicit authentication: Authentication): Either[IOError, VirtualFolder] = {
    val parentFolderE = toParentFolder(from)

    for {
      parentFolder <- parentFolderE
      foundFolder <- parentFolder.findFolder(name)
      file <- if (foundFolder.isEmpty) parentFolder.mkdir(name) else Right(foundFolder.get)
    } yield file
  }

}

case class AbsolutePath private (relativePath: RelativePath) extends VirtualPath {

  def name: String = if (relativePath.fragments.isEmpty) VirtualFS.pathSeparator else relativePath.fragments.reverse.head.toString

  override def parent: Option[AbsolutePath] =
    if (relativePath.fragments.size == 1)
      Some(VirtualPath.root)
    else
      relativePath.parent.map(path => AbsolutePath(path))

  def toParentFolder(fs: VirtualFS)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] =
    relativePath.toParentFolder(fs.root)

  def toFolder(fs: VirtualFS)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] =
    relativePath.toFolder(fs.root)

  def toFile(fs: VirtualFS)(implicit authentication: Authentication) : Either[IOError, VirtualFile] =
    relativePath.toFile(fs.root)

  def toParentFolder(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] =
    relativePath.toParentFolder(from.root)

  def toFolder(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] =
    relativePath.toFolder(from.root)

  def toFile(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFile] =
    relativePath.toFile(from.root)

  def andThen(fragment: PathFragment): AbsolutePath = AbsolutePath(relativePath.andThen(fragment))

  def andThen(path: RelativePath): AbsolutePath = AbsolutePath(relativePath.andThen(path))

  def andThen(fragments: String*): Either[IOError, AbsolutePath] =
    VirtualPath.relative(fragments:_*).right.map(path => andThen(path))

  override def toString: String = VirtualFS.pathSeparator + relativePath

}

case class RelativePath private (fragments: Seq[PathFragment]) extends VirtualPath {

  def name: String = if (fragments.isEmpty) "" else fragments.reverse.head.toString

  def toParentFolder(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] =
    for {
      path <- VirtualPath.of(from.path)
      parent <- path.andThen(this).parentOrError
      parentFolder <- parent.toFolder(from.root)
    } yield parentFolder

  def toFolder(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] =
    try {
      val folder: VirtualFolder = fragments.foldLeft(from)((current, fragment) => fragment match {
        case SelfFragment => current
        case ParentFragment => current.parent match {
          case Some(p) => p
          case _ => throw new IllegalStateException
        }
        case SimpleFragment(name) => current.findFolder(name) match {
          case Left(e) => throw new IllegalStateException(e.message)
          case Right(Some(f)) => f
          case Right(None) => throw new IllegalStateException
        }
      })
      Right(folder)
    } catch  {
      case _ : Exception => s"Invalid folder $this from $from".ioErrorE
    }

  def toFile(from: VirtualFolder)(implicit authentication: Authentication) : Either[IOError, VirtualFile] =
    toParentFolder(from).right.flatMap(_.findFile(name) match {
      case Right(Some(f)) => Right(f)
      case Right(None) => s"File '$this' not found from $from".ioErrorE
      case Left(e) => Left(e)
    })

  def andThen(fragment: PathFragment): RelativePath = RelativePath(fragments :+ fragment)

  def andThen(path: RelativePath): RelativePath = RelativePath(fragments ++ path.fragments)

  override def andThen(fragments: String*): Either[IOError, RelativePath] =
    VirtualPath.relative(fragments:_*).right.map(path => andThen(path))

  def parent: Option[RelativePath] =
    if (fragments.length == 1)
      None
    else
      Some(RelativePath(fragments.slice(0, fragments.length - 1)))

  override def toString: String = {
    fragments.foldLeft("")((oldValue, fragment) => {
      val prefix =
        if (oldValue.isEmpty || oldValue == VirtualFS.pathSeparator)
          ""
        else
          VirtualFS.pathSeparator

      oldValue + prefix + fragment.toString
    })
  }

}

object PathFragment {

  def of(fragment: String): Either[IOError, PathFragment] =
    if (fragment == VirtualFS.selfFragment)
      Right(SelfFragment)
    else if (fragment == VirtualFS.parentFragment)
      Right(ParentFragment)
    else
      try {
        Right(SimpleFragment(fragment))
      } catch {
        case it: Exception =>
          Left(IOError(it.getMessage))
      }

}

sealed trait PathFragment

object ParentFragment extends PathFragment {
  override def toString: String = VirtualFS.parentFragment
}

object SelfFragment extends PathFragment {
  override def toString: String = VirtualFS.selfFragment
}

case class SimpleFragment private(name: String) extends PathFragment {

  if (name.contains(VirtualFS.pathSeparator)) {
    // TODO I don't like to throw an exception, can I ignore this?
    throw new IllegalArgumentException("Illegal characters in name.")
  }

  override def toString: String = name
}