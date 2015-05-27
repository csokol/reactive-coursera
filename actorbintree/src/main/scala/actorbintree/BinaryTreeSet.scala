/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case msg: Operation => root ! msg
    case blob => println(s"don't know how to handle message ${blob}")
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, var initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, newElem) =>
      if (newElem == elem) {
        initiallyRemoved = false
        requester ! OperationFinished(id)
      } else {
        if (newElem > elem) {
          insertToChild(requester, id, newElem, Right)
        } else {
          insertToChild(requester, id, newElem, Left)
        }
      }
    case Contains(requester, id, queryElem) =>
      if (queryElem == elem) {
        val exists = !initiallyRemoved
        requester ! ContainsResult(id, exists)
      } else {
        if (queryElem > elem && subtrees.contains(Right)) {
          subtrees(Right) ! Contains(requester, id, queryElem)
        } else if (queryElem < elem && subtrees.contains(Left)) {
          subtrees(Left) ! Contains(requester, id, queryElem)
        } else {
          requester ! ContainsResult(id, false)
        }
      }
    case Remove(requester, id, target) =>
      if (target == elem) {
        initiallyRemoved = true
        requester ! OperationFinished(id)
      } else {
        if (target > elem && subtrees.contains(Right)) {
          subtrees(Right) ! Remove(requester, id, target)
        } else if (target < elem && subtrees.contains(Left)) {
          subtrees(Left) ! Remove(requester, id, target)
        } else {
          requester ! OperationFinished(id)
        }
      }
    case blob => println(s"don't know how to handle message ${blob}")
  }
  def insertToChild(requester: ActorRef, id: Int, newElem: Int, p:Position) {
    if (subtrees.contains(p)) {
      val rightActor: ActorRef = subtrees(p)
      rightActor ! Insert(requester, id, newElem)
    } else {
      val newRight: ActorRef = context.actorOf(props(newElem, false))
      subtrees = subtrees + (p -> newRight)
      newRight ! Insert(requester, id, newElem)
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???


}
