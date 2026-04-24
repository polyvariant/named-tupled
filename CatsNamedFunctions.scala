package namedfunctions

import scala.compiletime.erasedValue

import cats.Apply
import cats.Parallel

object catssyntax {

  type LiftTuple[F[_], Values <: Tuple] <: Tuple = Values match {
    case EmptyTuple => EmptyTuple
    case head *: tail => F[head] *: LiftTuple[F, tail]
  }

  trait LiftEvidence[Wrapped <: Tuple] {
    type F[_]
    type Values <: Tuple
  }

  object LiftEvidence {
    type Aux[Wrapped <: Tuple, F0[_], Values0 <: Tuple] =
      LiftEvidence[Wrapped] {
        type F[a] = F0[a]
        type Values = Values0
      }

    given base[F0[_], Head]: LiftEvidence[F0[Head] *: EmptyTuple] with {
      type F[a] = F0[a]
      type Values = Head *: EmptyTuple
    }

    given step[F0[_], Head, WrappedTail <: Tuple, ValuesTail <: Tuple](using
      tail: Aux[WrappedTail, F0, ValuesTail]
    ): LiftEvidence[F0[Head] *: WrappedTail] with {
      type F[a] = F0[a]
      type Values = Head *: ValuesTail
    }
  }

  private inline def tupledValues[F[_], Values <: Tuple](
    values: LiftTuple[F, Values]
  )(using F: Apply[F]): F[Values] =
    inline erasedValue[Values] match {
      case _: (head *: EmptyTuple) =>
        F.map(values.asInstanceOf[F[head] *: EmptyTuple].head)(headValue =>
          (headValue *: EmptyTuple).asInstanceOf[Values]
        )
      case _: (head *: tail) =>
        val tuple = values.asInstanceOf[F[head] *: LiftTuple[F, tail]]
        F.map(F.product(tuple.head, tupledValues[F, tail](tuple.tail))) { pair =>
          (pair._1 *: pair._2).asInstanceOf[Values]
        }
    }

  private inline def parTupledValues[F[_], Values <: Tuple](
    values: LiftTuple[F, Values]
  )(using P: Parallel[F]): F[Values] =
    inline erasedValue[Values] match {
      case _: (head *: EmptyTuple) =>
        P.monad.map(values.asInstanceOf[F[head] *: EmptyTuple].head)(headValue =>
          (headValue *: EmptyTuple).asInstanceOf[Values]
        )
      case _: (head *: tail) =>
        val tuple = values.asInstanceOf[F[head] *: LiftTuple[F, tail]]
        P.monad.map(cats.Parallel.parProduct(tuple.head, parTupledValues[F, tail](tuple.tail))(using P)) {
          pair =>
            (pair._1 *: pair._2).asInstanceOf[Values]
        }
    }

  extension [Names <: Tuple, Wrapped <: Tuple](nt: NamedTuple.NamedTuple[Names, Wrapped]) {

    inline def namedTupled(using ev: LiftEvidence[Wrapped], F: Apply[ev.F]): ev.F[
      NamedTuple.NamedTuple[Names, ev.Values]
    ] =
      F.map(tupledValues[ev.F, ev.Values](nt.asInstanceOf[LiftTuple[ev.F, ev.Values]])) { values =>
        values.asInstanceOf[NamedTuple.NamedTuple[Names, ev.Values]]
      }

    inline def namedMapN[R](using ev: LiftEvidence[Wrapped], F: Apply[ev.F])(
      f: NamedTuple.NamedTuple[Names, ev.Values] => R
    ): ev.F[R] =
      F.map(namedTupled(using ev, F))(f)

    inline def namedParMapN[R](using ev: LiftEvidence[Wrapped], P: Parallel[ev.F])(
      f: NamedTuple.NamedTuple[Names, ev.Values] => R
    ): ev.F[R] =
      P.monad.map(parTupledValues[ev.F, ev.Values](nt.asInstanceOf[LiftTuple[ev.F, ev.Values]])) {
        values =>
          f(values.asInstanceOf[NamedTuple.NamedTuple[Names, ev.Values]])
      }
  }

}
