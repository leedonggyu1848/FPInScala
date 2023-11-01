package org.example.ex2

object Solution {
    fun factorial(n: Int): Int {
        tailrec fun go(n: Int, acc: Int): Int =
            if (n <= 0) acc
            else go(n - 1, n * acc)
        return go(n, 1)
    }

    fun <A, B, C> partial1(a: A, f: (A, B) -> C): (B) -> C =
        { b: B -> f(a, b) }

    // 2.1
    fun fib(n: Int): Int {
        tailrec fun go(n: Int, prev: Int, cur: Int): Int =
            if (n <= 0) prev
            else go(n - 1, cur, prev + cur)
        return go(n, 0, 1)
    }

    // 2.2
    private val <T> List<T>.tail: List<T>
        get() = drop(1)
    private val <T> List<T>.head: T
        get() = first()
    fun <A> isSorted(aa: List<A>, order: (A, A) -> Boolean): Boolean =
        if (aa.size <= 1) true
        else order(aa.head, aa.tail.head) && isSorted(aa.tail, order)

    // 2.3
    fun <A, B, C> curry(f: (A, B) -> C): (A) -> (B) -> C =
        { a: A -> { b: B -> f(a, b) } }

    // 2.4
    fun <A, B, C> uncurry(f: (A) -> (B) -> C): (A, B) -> C =
        { a: A, b: B -> f(a)(b) }

    // 2.5
    fun <A, B, C> compose(f: (B) -> C, g: (A) -> B): (A) -> C =
        { a: A -> f(g(a)) }
}