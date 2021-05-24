object Permutation {
  def permute[A](l: List[A]): LazyList[List[A]] = l match {
    case Nil => LazyList(List())
    case _ => for ((x, xs) <- splits(l); perm <- permute(xs)) yield x::perm
  }

  /**
   * Returns a stream of all the tuples (a, as) obtained from the list "l"
   * where "a" is an element of "l" and "as" is the list "l" where "a" was
   * removed.
   */
  def splits[A](l: List[A]): LazyList[(A, List[A])] = l match {
    case Nil => LazyList()
    case x::xs => (splits(xs) map(e => (e._1, x::e._2))) :+ (x, xs)
  }

  def permute2[A](xs: List[A]): LazyList[List[A]] = xs match {
    case Nil => LazyList(List())
    case _ => for (x <- xs to LazyList; perm <- permute2(xs diff List(x))) yield x::perm
  }
}

object PermutationApp extends App{
  import Permutation._

  println(permute(List(1,2,3)).toList)
  println(permute2(List(1,2,3)).toList)

  println(permute(1 to 6 toList).size)
  println(permute2(1 to 6 toList).size)
}
