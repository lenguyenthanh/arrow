package arrow.mtl.extensions

import arrow.Kind
import arrow.core.Either
import arrow.core.ForId
import arrow.core.Id
import arrow.core.extensions.id.monad.monad
import arrow.extension
import arrow.mtl.IndexedStateT
import arrow.mtl.IndexedStateTPartialOf
import arrow.mtl.StateApi
import arrow.mtl.StateT
import arrow.mtl.StateTPartialOf
import arrow.mtl.extensions.indexedstatet.applicative.applicative
import arrow.mtl.extensions.indexedstatet.functor.functor
import arrow.mtl.extensions.indexedstatet.monad.monad
import arrow.mtl.fix
import arrow.mtl.runM
import arrow.typeclasses.Applicative
import arrow.typeclasses.ApplicativeError
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.MonadError
import arrow.typeclasses.SemigroupK

@extension
interface IndexedStateTFunctorInstance<F, S> : Functor<IndexedStateTPartialOf<F, S, S>> {

  fun FF(): Functor<F>

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.map(f: (A) -> B): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().map(FF(), f)

}

@extension
interface IndexedStateTApplicativeInstance<F, S> : Applicative<IndexedStateTPartialOf<F, S, S>>, IndexedStateTFunctorInstance<F, S> {

  fun MF(): Monad<F>

  override fun FF(): Monad<F> = MF()

  override fun <A> just(a: A): Kind<IndexedStateTPartialOf<F, S, S>, A> =
    IndexedStateT.just(MF(), a)

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.ap(ff: Kind<IndexedStateTPartialOf<F, S, S>, (A) -> B>): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().ap(MF(), ff)

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.map(f: (A) -> B): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().map(MF(), f)

}

@extension
interface IndexedStateTMonadInstance<F, S> : Monad<IndexedStateTPartialOf<F, S, S>>, IndexedStateTFunctorInstance<F, S> {

  override fun FF(): Monad<F>

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.flatMap(f: (A) -> Kind<IndexedStateTPartialOf<F, S, S>, B>): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().flatMap(FF(), f)

  override fun <A, B> tailRecM(a: A, f: (A) -> Kind<IndexedStateTPartialOf<F, S, S>, Either<A, B>>): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    IndexedStateT.tailRecM(FF(), a, f)

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.map(f: (A) -> B): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().map(FF(), f)

  override fun <A> just(a: A): Kind<IndexedStateTPartialOf<F, S, S>, A> =
    IndexedStateT.just(FF(), a)

}

@extension
interface IndexedStateTSemigroupKInstance<F, SA, SB> : SemigroupK<IndexedStateTPartialOf<F, SA, SB>> {

  fun MF(): Monad<F>

  fun SS(): SemigroupK<F>

  override fun <A> Kind<IndexedStateTPartialOf<F, SA, SB>, A>.combineK(y: Kind<IndexedStateTPartialOf<F, SA, SB>, A>): Kind<IndexedStateTPartialOf<F, SA, SB>, A> =
    fix().combineK(y, MF(), SS())

}

@extension
interface IndexedStateTApplicativeErrorInstance<F, S, E> : ApplicativeError<IndexedStateTPartialOf<F, S, S>, E>, IndexedStateTApplicativeInstance<F, S> {

  override fun MF(): Monad<F> = FF()

  override fun FF(): MonadError<F, E>

  override fun <A> raiseError(e: E): Kind<IndexedStateTPartialOf<F, S, S>, A> = IndexedStateT.lift(FF(), FF().raiseError(e))

  override fun <A> Kind<StateTPartialOf<F, S>, A>.handleErrorWith(f: (E) -> Kind<IndexedStateTPartialOf<F, S, S>, A>): StateT<F, S, A> =
    IndexedStateT(FF().just({ s -> FF().run { runM(FF(), s).handleErrorWith { e -> f(e).runM(FF(), s) } } }))

}

@extension
interface IndexedStateTMonadErrorInstance<F, S, E> : MonadError<IndexedStateTPartialOf<F, S, S>, E> , IndexedStateTApplicativeErrorInstance<F, S, E>, IndexedStateTMonadInstance<F, S> {

  override fun FF(): MonadError<F, E>

  override fun <A> just(a: A): Kind<IndexedStateTPartialOf<F, S, S>, A> = IndexedStateT.just(FF(), a)

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.ap(ff: Kind<IndexedStateTPartialOf<F, S, S>, (A) -> B>): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().ap(FF(), ff)

  override fun <A, B> Kind<IndexedStateTPartialOf<F, S, S>, A>.map(f: (A) -> B): Kind<IndexedStateTPartialOf<F, S, S>, B> =
    fix().map(FF(), f)

}

fun <F, S, E> IndexedStateT.Companion.monadError(FF: arrow.typeclasses.MonadError<F, E>, @Suppress("UNUSED_PARAMETER") dummy: kotlin.Unit = kotlin.Unit): IndexedStateTMonadErrorInstance<F, S, E> =
  object : IndexedStateTMonadErrorInstance<F, S, E> {
    override fun FF(): arrow.typeclasses.MonadError<F, E> = FF
    override fun MF(): Monad<F> = FF
  }

/**
 * Alias for[StateT.Companion.applicative]
 */
fun <S> StateApi.applicative(): Applicative<StateTPartialOf<ForId, S>> = StateT.applicative<ForId, S>(Id.monad())

/**
 * Alias for [StateT.Companion.functor]
 */
fun <S> StateApi.functor(): Functor<StateTPartialOf<ForId, S>> = StateT.functor<ForId, S>(Id.monad())

/**
 * Alias for [StateT.Companion.monad]
 */
fun <S> StateApi.monad(): Monad<StateTPartialOf<ForId, S>> = StateT.monad<ForId, S>(Id.monad())

class IndexedStateTContext<F, S, E>(val ME: MonadError<F, E>) : IndexedStateTMonadErrorInstance<F, S, E> {
  override fun MF(): Monad<F> = ME
  override fun FF(): MonadError<F, E> = ME
}

class IndexedStateTContextPartiallyApplied<F, S, E>(val ME: MonadError<F, E>) {
  infix fun <A> extensions(f: IndexedStateTContext<F, S, E>.() -> A): A =
    f(IndexedStateTContext(ME))
}

fun <F, S, E> ForIndexedStateT(ME: MonadError<F, E>): IndexedStateTContextPartiallyApplied<F, S, E> =
  IndexedStateTContextPartiallyApplied(ME)

fun <F, S, E> ForStateT(ME: MonadError<F, E>): IndexedStateTContextPartiallyApplied<F, S, E> =
  IndexedStateTContextPartiallyApplied(ME)

class StateTMonadContext<S> : IndexedStateTMonadInstance<ForId, S> {
  override fun FF(): Monad<ForId> = Id.monad()
}

class StateContextPartiallyApplied<S> {
  infix fun <A> extensions(f: StateTMonadContext<S>.() -> A): A =
    f(StateTMonadContext())
}

fun <S> ForState(): StateContextPartiallyApplied<S> =
  StateContextPartiallyApplied()
