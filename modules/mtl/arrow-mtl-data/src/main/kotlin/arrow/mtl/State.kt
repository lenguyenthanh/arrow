package arrow.mtl

import arrow.core.*
import arrow.typeclasses.internal.IdBimonad

/**
 * Alias that represents stateful computation of the form `(S) -> Tuple2<S, A>`.
 */
typealias StateFun<S, A> = StateTFun<ForId, S, A>

/**
 * Alias that represents wrapped stateful computation in context `Id`.
 */
typealias StateFunOf<S, A> = StateTFunOf<ForId, S, A>

/**
 * Alias for StateHK
 */
typealias ForState = ForStateT

/**
 * Alias for StateKind
 */
typealias StateOf<S, A> = StateTOf<ForId, S, A>

/**
 * Alias to partially apply type parameters [S] to [State]
 */
typealias StatePartialOf<S> = StateTPartialOf<ForId, S>

/**
 * `State<S, A>` is a stateful computation that yields a value of type `A`.
 *
 * @param S the state we are preforming computation upon.
 * @param A current value of computation.
 */
typealias State<S, A> = StateT<ForId, S, A>

/**
 * Constructor for State.
 * State<S, A> is an alias for IndexedStateT<ForId, S, S, A>
 *
 * @param run the stateful function to wrap with [State].
 */
@Suppress("FunctionName")
fun <S, A> State(run: (S) -> Tuple2<S, A>): State<S, A> = StateT(Id(run.andThen { Id(it) }))

/**
 * Syntax for constructing a `StateT<ForId, S, A>` from a function `(S) -> Tuple2<S, A>`
 */
fun <S, A> StateFun<S, A>.toState(): State<S, A> = State(IdBimonad, this)

/**
 * Syntax for constructing a `StateT<ForId, S, A>` from a function `(S) -> Tuple2<S, A>`
 */
fun <S, A> StateFunOf<S, A>.toState(): State<S, A> = State(this)

fun <S, T, P1, R> State<S, T>.map(sx: State<S, P1>, f: (T, P1) -> R): State<S, R> =
  flatMap(IdBimonad) { t -> sx.map { x -> f(t, x) } }.fix()

fun <S, T, R> State<S, T>.map(f: (T) -> R): State<S, R> = flatMap(IdBimonad) { t -> StateApi.just<S, R>(f(t)) }.fix()

/**
 * Alias for [StateT.run] `StateT<ForId, S, A>`
 *
 * @param initial state to start stateful computation.
 */
fun <S, A> StateT<ForId, S, A>.run(initial: S): Tuple2<S, A> = run(IdBimonad, initial).value()

/**
 * Alias for [StateT.runA] `StateT<ForId, S, A>`
 *
 * @param initial state to start stateful computation.
 */
fun <S, A> StateT<ForId, S, A>.runA(initial: S): A = run(initial).b

/**
 * Alias for [StateT.runS] `StateT<ForId, S, A>`
 *
 * @param initial state to start stateful computation.
 */
fun <S, A> StateT<ForId, S, A>.runS(initial: S): S = run(initial).a

typealias StateApi = IndexedStateApi

/**
 * Alias for StateId to make working with `StateT<ForId, S, A>` more elegant.
 */
@Suppress("FunctionName")
fun State() = StateApi

fun <R, S, T> List<T>.stateTraverse(f: (T) -> State<S, R>): State<S, List<R>> = foldRight(StateApi.just(emptyList())) { i: T, accumulator: State<S, List<R>> ->
  f(i).map(accumulator, ({ head: R, tail: List<R> ->
    listOf(head) + tail
  }))
}

fun <S, T> List<State<S, T>>.stateSequential(): State<S, List<T>> = stateTraverse(::identity)

