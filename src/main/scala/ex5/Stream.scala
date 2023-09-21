package ex5

enum Stream[+A]:
  case Cons(h: () => A, t: () => Stream[A])
  case Empty extends Stream[Nothing]

object Stream:
  ???