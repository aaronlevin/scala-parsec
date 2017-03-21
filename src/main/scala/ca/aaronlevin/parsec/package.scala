/*
 * Copyright (c) 2017 Aaron Levin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package ca.aaronlevin

import cats.{ Functor, Id, Monad }
import cats.implicits._
import scala.language.higherKinds

package object parsec {

  type SourceName = String
  type Line       = Int
  type Column     = Int

  case class SourcePosition(sourceName: SourceName, line: Line, column: Column)
  case class ParseError(sourcePos: SourcePosition, messages: List[String])

  case class State[S, U](input: S, pos: SourcePosition, user: U)

  sealed trait IConsumed[A]
  case class Consumed[A](consumed: A) extends IConsumed[A]
  case class Empty[A](empty: A)       extends IConsumed[A]

  sealed trait Reply[+S, +U, +A]
  case class Ok[S, U, A](a: A, state: State[S, U], error: ParseError) extends Reply[S, U, A]
  case class Error(error: ParseError)                                 extends Reply[Nothing, Nothing, Nothing]

  def mergeError(pe1: ParseError, pe2: ParseError): ParseError = pe1
  def newErrorUnknown(pos: SourcePosition): ParseError         = ParseError(pos, Nil)
  def unknownError[S, U](state: State[S, U]): ParseError       = newErrorUnknown(state.pos)
  def initialPos(name: SourceName): SourcePosition             = SourcePosition(name, 1, 1)

  trait ParsecStream[S, M[_], Token] {
    implicit def M: Monad[M]
    def uncons(stream: S): M[Option[(Token, S)]]
  }

  implicit def listStream[T, M[_]](implicit monad: Monad[M]) = new ParsecStream[List[T], M, T] {
    implicit def M = monad
    def uncons(stream: List[T]): M[Option[(T, List[T])]] = stream match {
      case Nil     => monad.pure(None)
      case t :: ts => monad.pure(Some((t, ts)))
    }
  }

  abstract class ParsecT[S, U, M[_], A] {
    def apply[B](
        state: State[S, U],
        consumedOk: (A, State[S, U], ParseError) => M[B],
        consumedErr: ParseError => M[B],
        emptyOk: (A, State[S, U], ParseError) => M[B],
        emptyErr: ParseError => M[B]
    ): M[B]

    def map[B](f: A => B): ParsecT[S, U, M, B] = parsecFunctor.map(this)(f)
  }

  implicit def parsecFunctor[S, U, M[_]]: Functor[ParsecT[S, U, M, ?]] =
    new Functor[ParsecT[S, U, M, ?]] {
      def map[A, B](fa: ParsecT[S, U, M, A])(f: A => B): ParsecT[S, U, M, B] =
        new ParsecT[S, U, M, B] {
          def apply[C](state: State[S, U],
                       consumedOk: (B, State[S, U], ParseError) => M[C],
                       consumedErr: ParseError => M[C],
                       emptyOk: (B, State[S, U], ParseError) => M[C],
                       emptyErr: ParseError => M[C]): M[C] =
            fa.apply(
              state, { (a, s, e) =>
                consumedOk(f(a), s, e)
              },
              consumedErr, { (a, s, e) =>
                emptyOk(f(a), s, e)
              },
              emptyErr
            )
        }
    }

  implicit def parsecMonad[S, U, M[_]]: Monad[ParsecT[S, U, M, ?]] =
    new Monad[ParsecT[S, U, M, ?]] {

      def flatMap[A, B](
          fa: ParsecT[S, U, M, A]
      )(f: A => ParsecT[S, U, M, B]): ParsecT[S, U, M, B] =
        new ParsecT[S, U, M, B] {
          def apply[C](state: State[S, U],
                       consumedOk: (B, State[S, U], ParseError) => M[C],
                       consumedErr: ParseError => M[C],
                       emptyOk: (B, State[S, U], ParseError) => M[C],
                       emptyErr: ParseError => M[C]): M[C] = {

            // consumed ok case
            val mcOk: (A, State[S, U], ParseError) => M[C] = { (a, s, e) =>
              val peok: (B, State[S, U], ParseError) => M[C] = { (pa, ps, pe) =>
                consumedOk(pa, ps, mergeError(e, pe))
              }
              val peer: ParseError => M[C] = { pe =>
                consumedErr(mergeError(e, pe))
              }
              f(a).apply[C](s, consumedOk, consumedErr, peok, peer)
            }

            // empty ok case
            val meok: (A, State[S, U], ParseError) => M[C] = { (a, s, e) =>
              val peok: (B, State[S, U], ParseError) => M[C] = { (pa, ps, pe) =>
                emptyOk(pa, ps, mergeError(e, pe))
              }
              val peer: ParseError => M[C] = { pe =>
                emptyErr(mergeError(e, pe))
              }
              f(a).apply[C](s, consumedOk, consumedErr, peok, peer)
            }
            fa.apply[C](state, mcOk, consumedErr, meok, emptyErr)
          }
        }

      def pure[A](a: A): ParsecT[S, U, M, A] = new ParsecT[S, U, M, A] {
        def apply[B](state: State[S, U],
                     consumedOk: (A, State[S, U], ParseError) => M[B],
                     consumedErr: ParseError => M[B],
                     emptyOk: (A, State[S, U], ParseError) => M[B],
                     emptyErr: ParseError => M[B]): M[B] =
          emptyOk(a, state, unknownError(state))
      }

      def tailRecM[A, B](a: A)(f: A => ParsecT[S, U, M, Either[A, B]]): ParsecT[S, U, M, B] =
        new ParsecT[S, U, M, B] {
          def apply[C](state: State[S, U],
                       consumedOk: (B, State[S, U], ParseError) => M[C],
                       consumedErr: ParseError => M[C],
                       emptyOk: (B, State[S, U], ParseError) => M[C],
                       emptyErr: ParseError => M[C]): M[C] = {

            val mcOk: (Either[A, B], State[S, U], ParseError) => M[C] = { (eab, s, e) =>
              eab match {
                case Left(la) =>
                  tailRecM(la)(f).apply(state, consumedOk, consumedErr, emptyOk, emptyErr)
                case Right(b) => consumedOk(b, s, e)
              }
            }
            val meok: (Either[A, B], State[S, U], ParseError) => M[C] = { (eab, s, e) =>
              eab match {
                case Left(la) =>
                  tailRecM(la)(f).apply(state, consumedOk, consumedErr, emptyOk, emptyErr)
                case Right(b) => emptyOk(b, s, e)
              }
            }
            f(a).apply[C](state, mcOk, consumedErr, meok, emptyErr)
          }
        }
    }

  def runParsecT[S, U, M[_], A](parser: ParsecT[S, U, M, A], state: State[S, U])(
      implicit monad: Monad[M]
  ): M[IConsumed[M[Reply[S, U, A]]]] = {
    val cok: (A, State[S, U], ParseError) => M[IConsumed[M[Reply[S, U, A]]]] = { (a, s, e) =>
      monad.pure(Consumed(monad.pure(Ok(a, s, e))))
    }
    val cerr: ParseError => M[IConsumed[M[Reply[S, U, A]]]] = { e =>
      monad.pure(Consumed(monad.pure(Error(e))))
    }
    val eok: (A, State[S, U], ParseError) => M[IConsumed[M[Reply[S, U, A]]]] = { (a, s, e) =>
      monad.pure(Empty(monad.pure(Ok(a, s, e))))
    }
    val eerr: ParseError => M[IConsumed[M[Reply[S, U, A]]]] = { e =>
      monad.pure(Empty(monad.pure(Error(e))))
    }

    parser.apply(state, cok, cerr, eok, eerr)
  }

  def runPT[S, M[_], T, U, A](parser: ParsecT[S, U, M, A], user: U, sourceName: SourceName, s: S)(
      implicit monad: Monad[M]
  ): M[Either[ParseError, A]] = {
    val state = State(s, initialPos(sourceName), user)
    monad.flatMap(runParsecT(parser, state)) { consumedRes =>
      val result = consumedRes match {
        case Empty(e)    => e
        case Consumed(c) => c
      }
      monad.map(result) {
        case Ok(x, _, _) => Right(x)
        case Error(err)  => Left(err)
      }
    }
  }

  def parse[S, T, A](parser: ParsecT[S, Unit, Id, A], source: SourceName, s: S)(
      implicit stream: ParsecStream[S, Id, T]
  ): Either[ParseError, A] =
    runPT[S, Id, T, Unit, A](parser, Unit, source, s)

  // this will blow the stack at n=9600 on my machine
  def stackTest(n: Int) = {
    val monad                              = parsecMonad[Char, Unit, List]
    val r                                  = monad.pure(333)
    val rr: ParsecT[Char, Unit, List, Int] = (1 to n).foldLeft(r)((r, _) => r.map(_ + 1))

    runPT[Char, List, Char, Unit, Int](rr, Unit, "aaron", 'c')
  }
}
