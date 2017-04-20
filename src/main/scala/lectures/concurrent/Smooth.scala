package lectures.concurrent

import scala.concurrent.{Future, Promise}

/**
  * Smooth - это своебразный функциональный кэш, предназначенный для исключения повторных вызовов кода
  * до того, как получен результат первого вызова.
  * Он работает следующим образом:
  * * * * в объект Smooth в метод apply передается код, который может выполняться какое-то время, и возвращает какое-то значение
  * * * * apply создаст инстанс Smooth
  * * * * созданный инстанс при вызове apply возвращает Future
  * * * * * и запускает код, если код еще не запущен
  * * * * * и не запускает код, если код еще не завершился с момента предыдущего запуска
  *
  * Подсказка: можно использовать AtomicReference
  *
  */
object Smooth {
  def apply[T](thunk: => T): Smooth[T] = new Smooth[T](thunk)
}

class Smooth[T](thunk: => T) {
  var cur = Option.empty[Future[T]]

  def apply(): Future[T] = this.synchronized {
    cur match {
      case Some(x) => x
      case None =>
        val f = Future(thunk).andThen { case _ => cur = None }
        cur = Some(f)
        f
    }
  }
}
