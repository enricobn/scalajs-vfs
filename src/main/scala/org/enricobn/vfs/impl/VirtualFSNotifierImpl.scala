package org.enricobn.vfs.impl

import org.enricobn.vfs.{VirtualFSNotifier, VirtualFSNotifierPub, VirtualNode}

import scala.collection.mutable

class VirtualFSNotifierImpl extends VirtualFSNotifier {

  private val publishers = mutable.HashMap[String, VirtualFSNotifierPub]()

  override def addWatch(node: VirtualNode, subscriber: VirtualFSNotifierPub#Sub) : Unit = {
    val pub = publishers.getOrElseUpdate(node.path, { new VirtualFSNotifierPub()} )
    pub.subscribe(subscriber)
  }

  override def notify(node: VirtualNode): Unit = {
    if (publishers.contains(node.path)) {
      val pub = publishers(node.path)
      pub.publish(())
    }
  }

  override def removeWatch(node: VirtualNode, subscriber: VirtualFSNotifierPub#Sub): Unit = {
    if (publishers.contains(node.path)) {
      val pub = publishers(node.path)
      pub.removeSubscription(subscriber)
    }
  }

  override def shutdown(): Unit = {
    publishers.foreach(_._2.removeSubscriptions())
    publishers.clear()
  }
}
