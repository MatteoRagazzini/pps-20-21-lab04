package u04lab.code

import Optionals._
import Lists._
import List.{Cons, Nil, append, nil, reverse}
import Streams._
import Streams.Stream._


import scala.util.Random

object StreamUtils{
  def fromList[A](list: List[A]): Stream[A] = list match {
    case Cons(h,t) => Stream.cons(h, fromList(t))
    case _ => Stream.empty()
  }
}

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

case class PowerIteratorsFromStream[A](var stream: Stream[A]) extends PowerIterator[A]{
  var pastList: List[A] = nil[A]

  override def next(): Option[A] = stream match{
    case Stream.Cons(h, t)=> {
      stream = t()
      pastList = Cons(h(), pastList)
      Option.of(h())
    }
    case Empty() => Option.empty
  }

  override def allSoFar(): List[A] = reverse(pastList)

  override def reversed(): PowerIterator[A] = PowerIteratorsFromStream(StreamUtils.fromList(pastList))
}


class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = PowerIteratorsFromStream(iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIteratorsFromStream(StreamUtils.fromList(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIteratorsFromStream(take(generate(Random.nextBoolean()))(size))
}
