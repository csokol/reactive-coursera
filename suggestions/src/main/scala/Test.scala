import rx.lang.scala.subjects.{ReplaySubject, PublishSubject}
import rx.lang.scala.{Observer, Observable}
import suggestions.gui.WikipediaApi

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Test extends App{

  import rx.lang.scala.subjects.PublishSubject
  import scala.language.postfixOps
  import scala.concurrent.duration._

  val s = PublishSubject[Int]()

  s.take(1 second).subscribe(t => println(s"gotcha: ${t}"))
  s.subscribe(println(_))

  s.onNext(10)
  s.onNext(10)

  Thread.sleep(2000)
  s.onNext(666)

}
