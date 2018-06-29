package org.enricobn.vfs

import scala.collection.mutable

trait VirtualFSNotifier {

  def addWatch(node: VirtualNode, subscriber: VirtualFSNotifierPub#Sub) : Unit

  def removeWatch(node: VirtualNode, subscriber: VirtualFSNotifierPub#Sub): Unit

  def notify(node: VirtualNode) : Unit

  def shutdown() : Unit

}

class VirtualFSNotifierPub extends mutable.Publisher[Unit] {

  override type Pub = mutable.Publisher[Unit]

  override def publish(event: Unit): Unit = super.publish(event)

}