package nodescala

import scala.async.Async
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.io.StdIn
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

object Test extends App {

  val v0 = System.currentTimeMillis()
  val working = Future.run() { ct =>
    Future {
      while (ct.nonCancelled) {
        println("working")
      }
      println("done")
    }
  }
  Future.delay(5 seconds) onSuccess {
    case _ => {
      working.unsubscribe()
      val v1 = System.currentTimeMillis()
      println(s"worked for ${v1 - v0}")
    }
  }

  Thread.sleep(6000)
}
