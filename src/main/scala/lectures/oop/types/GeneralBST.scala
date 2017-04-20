package lectures.oop.types

import lectures.oop.BSTImpl

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[+T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add[U >: T : Ordering](newValue: U): GeneralBST[U]

  def find[U >: T : Ordering](value: U): Option[GeneralBST[T]]
}

case class GeneralBSTImpl[+T](value: T, left: Option[GeneralBST[T]], right: Option[GeneralBST[T]]) extends GeneralBST[T] {

  override def add[U >: T](newValue: U)(implicit ordering: Ordering[U]): GeneralBST[U] = {

    import ordering.mkOrderingOps

    this match {
      case GeneralBSTImpl(`newValue`, _, _) => this
      case GeneralBSTImpl(x, None, _) if x < newValue => copy(left = Some(GeneralBSTImpl(newValue, None, None)))
      case GeneralBSTImpl(x, _, None) if x > newValue => copy(right = Some(GeneralBSTImpl(newValue, None, None)))
      case GeneralBSTImpl(x, Some(l), _) if x < newValue => copy(left = Some(l.add(newValue)))
      case GeneralBSTImpl(x, _, Some(r)) if x > newValue => copy(right = Some(r.add(newValue)))
    }
  }

  override def find[U >: T](value: U)(implicit ordering: Ordering[U]): Option[GeneralBST[T]] = {

    import ordering.mkOrderingOps

    value match {
      case `value` => Some(this)
      case x if x < value => left.flatMap(_.find(x))
      case x if x > value => right.flatMap(_.find(x))
    }

  }
}