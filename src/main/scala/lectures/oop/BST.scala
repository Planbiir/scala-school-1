package lectures.oop


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def toString(tree: BST): Unit

}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BSTImpl = this match {
    case BSTImpl(`newValue`, _, _) => this
    case BSTImpl(x, None, _) if x < newValue => copy(left = Some(BSTImpl(newValue, None, None)))
    case BSTImpl(x, _, None) if x > newValue => copy(right = Some(BSTImpl(newValue, None, None)))
    case BSTImpl(x, Some(l), _) if x < newValue => copy(left = Some(l.add(newValue)))
    case BSTImpl(x, _, Some(r)) if x > newValue => copy(right = Some(r.add(newValue)))
  }

  def find(value: Int): Option[BST] = value match {
    case `value` => Some(this)
    case x if x < value => left.flatMap(_.find(x))
    case x if x > value => right.flatMap(_.find(x))
  }

    override def toString(tree: BST) = ???

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount: Int = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = (1 to nodesCount).foldLeft(root)((x, _) => x.add((Math.random() * maxValue).toInt))
  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)
  require(testTree.find(markerItem3).isDefined)

  println(testTree)
}