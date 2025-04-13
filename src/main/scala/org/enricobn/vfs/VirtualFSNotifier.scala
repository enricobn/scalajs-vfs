package org.enricobn.vfs

trait VirtualFSNotifier {

  def addWatch(node: VirtualNode, subscriber: Unit => Unit) : Unit

  def removeWatch(node: VirtualNode, subscriber: Unit => Unit): Unit

  def notify(node: VirtualNode) : Unit

  def shutdown() : Unit

}

class VirtualFSNotifierPub extends Listeners[Unit] {

}