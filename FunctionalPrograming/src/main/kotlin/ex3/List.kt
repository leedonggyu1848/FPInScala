package org.example.ex3

sealed class List<out A> {
    companion object {
        fun <A> of(vararg aa: A): List<A> {
            val tail = aa.sliceArray(1 until aa.size)
            return if (aa.isEmpty()) Nil else Cons(aa[0], of(*tail))
        }

        fun sum(ints: List<Int>): Int =
            foldRight(ints, 0) { a, b -> a + b }

        fun product(dbs: List<Double>): Double =
            foldRight(dbs, 1.0) { a, b -> a * b }


        fun <A, B> foldRight(xs: List<A>, z: B, f: (A, B) -> B): B = when (xs) {
            is Nil -> z
            is Cons -> f(xs.head, foldRight(xs.tail, z, f))
        }


        fun <A> append(a1: List<A>, a2: List<A>): List<A> = when (a1) {
            is Nil -> a2
            is Cons -> Cons(a1.head, append(a1.tail, a2))
        }

        fun <A> empty(): List<A> = Nil

        // 3.1
        fun <A> tail(xs: List<A>): List<A> = when (xs) {
            is Nil -> Nil
            is Cons -> xs.tail
        }

        // 3.2
        fun <A> setHead(xs: List<A>, x: A): List<A> = when (xs) {
            is Nil -> Cons(x, Nil)
            is Cons -> Cons(x, xs)
        }

        // 3.3
        fun <A> drop(l: List<A>, n: Int): List<A> = when (l) {
            is Nil -> Nil
            is Cons -> if (n == 0) l else drop(l.tail, n - 1)
        }

        // 3.4
        fun <A> dropWhile(l: List<A>, f: (A) -> Boolean): List<A> = when (l) {
            is Nil -> Nil
            is Cons -> if (f(l.head)) dropWhile(l.tail, f) else l
        }

        // 3.5
        fun <A> init(l: List<A>): List<A> = when (l) {
            is Nil -> Nil
            is Cons -> if (l.tail == Nil) Nil else Cons(l.head, init(l.tail))
        }

        // 3.8
        fun <A> length(xs: List<A>): Int =
            foldRight(xs, 0) { _, acc -> acc + 1 }

        // 3.9
        tailrec fun <A, B> foldLeft(xs: List<A>, z: B, f: (B, A) -> B): B = when (xs) {
            is Nil -> z
            is Cons -> foldLeft(xs.tail, f(z, xs.head), f)
        }

        // 3.10
        fun sumViaFoldLeft(xs: List<Int>): Int =
            foldLeft(xs, 0) { a, b -> a + b }

        fun productViaFoldLeft(xs: List<Double>): Double =
            foldLeft(xs, 1.0) { a, b -> a * b }

        fun <A> lengthViaFoldLeft(xs: List<A>): Int =
            foldLeft(xs, 0) { acc, _ -> acc + 1 }

        // 3.11
        fun reverse(xs: List<Int>): List<Int> =
            foldLeft(xs, empty()) { acc, x -> Cons(x, acc) }

        // 3.12
        fun <A, B> foldRightViaFoldLeft(xs: List<A>, z: B, f: (A, B) -> B): B =
            foldLeft(xs, { x:B -> x } ) { idf, a -> { b -> idf(f(a, b)) }  } (z)

        // 3.13
        fun <A> appendViaFoldRight(a1: List<A>, a2: List<A>): List<A> =
            foldRightViaFoldLeft(a1, a2) { x, acc -> Cons(x, acc) }

        // 3.14
        fun <A> concat(xs: List<List<A>>): List<A> =
            foldRightViaFoldLeft(xs, empty(), ::appendViaFoldRight)

        // 3.15
        fun addOneAllElements(xs: List<Int>): List<Int> =
            foldRightViaFoldLeft(xs, empty()) { x, acc -> Cons(x + 1, acc) }

        // 3.16
        fun doubleToString(xs: List<Double>): List<String> =
            foldRightViaFoldLeft(xs, empty()) { x, acc -> Cons(x.toString(), acc) }

        // 3.17
        fun <A, B> map(xs: List<A>, f: (A) -> B): List<B> =
            foldRightViaFoldLeft(xs, empty()) { x, acc -> Cons(f(x), acc) }

        // 3.18
        fun <A> filter(xs: List<A>, f: (A) -> Boolean): List<A> =
            foldRightViaFoldLeft(xs, empty()) { x, acc -> if (f(x)) Cons(x, acc) else acc }

        // 3.19
        fun <A, B> flatMap(xs: List<A>, f: (A) -> List<B>): List<B> =
            concat(map(xs, f))

        // 3.20
        fun <A> filterViaFlatMap(xs: List<A>, f: (A) -> Boolean): List<A> =
            flatMap(xs) { if (f(it)) List.of(it) else empty() }
    }
}

data object Nil: List<Nothing>()

data class Cons<A>(val head: A, val tail: List<A>): List<A>()
fun <A> List<A>.tail(): List<A> = List.tail(this)