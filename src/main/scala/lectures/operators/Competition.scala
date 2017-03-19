package lectures.operators

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[(String, Int)] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом числовое значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать, чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо напечатать:
  * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * "Продули", если победили приезжие
  * "Победила дружба" в случае ничьи
  *
  * Для решения задачи раскомментируйте тело объекта Competition
  */

object Competition extends App {

  val locals: Map[String, Int] = Map("Artem" -> 6, "Sergey" -> 5, "Anton" -> 2, "Vladimir" -> 2, "Alexander" -> 41)
  val foreigners: Map[String, Int] = Map("John" -> 3, "James" -> 1, "Tom" -> 2, "Dick" -> 5, "Eric" -> 6)

  val results =
    for (a1 <- locals;
         a2 <- foreigners)
      yield {
        val localName = a1._1
        val localValue: Int = a1._2.toInt
        val foreignerName = a2._1
        val foreignerValue: Int = a2._2.toInt
        val y = localValue - foreignerValue
        localName + " " + foreignerName -> y
      }

  var finalResult = 0
  for (r <- results) yield {
    val localValue = r._2.toInt
    if (localValue > 0) finalResult = finalResult + 1
    else if (localValue < 0) finalResult = finalResult - 1
  }
  if (finalResult > 0) print("Победили наши")
  else if (finalResult > 0) print("Победили соперники")
  else print("Победила дружба")
}

