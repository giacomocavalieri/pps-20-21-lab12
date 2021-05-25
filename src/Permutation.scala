object Permutation {
  // member([X|Xs], X, Xs).
  // member([X|Xs], E, [X|Ys]) :- member(Xs, E, Ys).
  def member[A](l: List[A]): LazyList[(A, List[A])] = l match {
    case Nil => LazyList()
    case x::xs => (for((e, ys) <- member(xs)) yield (e, x::ys)) :+ (x,xs)
  }

  // permutation([],[]).
  // permutation(Xs, [X|Ys]) :- member(Xs, X, Zs), permutation(Zs, Ys).
  def permutation[A](xs: List[A]): LazyList[List[A]] = xs match {
    case Nil => LazyList(List())
    case _ => for ((x, zs) <- member(xs); ys <- permutation(zs)) yield x::ys
  }
}

object PermutationApp extends App{
  import Permutation._

  println(permutation(List(1,2,3)).toList)
  println(permutation(1 to 6 toList).size)
}
