package object skat {

  implicit class Nihilist[T](l: List[T]) {
    def notExists(p: T => Boolean): Boolean =
      !l.exists(p)

    def -(t: T): List[T] =
      l.filter(_ != t)

    def --(l2: List[T]): List[T] =
      l.filter(!l2.contains(_))
  }

}
