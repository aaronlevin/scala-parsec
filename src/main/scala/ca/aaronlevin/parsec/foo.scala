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

package ca.aaronlevin.parsec

import cats.{ Functor, Id, Monad }
import cats.implicits._
import scala.language.higherKinds
import scala.annotation.tailrec

object StacklessTest {

  // easiest way to express rank-2 polymorphism...
  trait IFoo[S, M[_], A] {
    def apply[B](state: S, cont: A => M[B]): M[B]
  }

  // A stripped down ParsecT
  sealed abstract class Foo[S, M[_], A] {
    def apply[B](state: S, cont: A => M[B]): M[B] = {
      import Foo._
      val renv = new Foo.RunEnv[M, B]
      renv.eval(state, this, renv.ScalaFunc(cont)).get
    }

    final def map[B](f: A => B): Foo[S, M, B]                = Foo.Mapped(this, f)
    final def flatMap[B](f: A => Foo[S, M, B]): Foo[S, M, B] = Foo.FlatMapped(this, f)
  }

  object Foo {

    case class Pure[S, M[_], A](value: A)                           extends Foo[S, M, A]
    case class Mapped[S, M[_], A, B](foo: Foo[S, M, A], fn: A => B) extends Foo[S, M, B]
    case class FlatMapped[S, M[_], A, B](foo: Foo[S, M, A], fn: A => Foo[S, M, B])
        extends Foo[S, M, B]
    case class F[S, M[_], A](f: IFoo[S, M, A]) extends Foo[S, M, A]

    // START: RunEnv
    private class RunEnv[M[_], B] {

      /**
        * Result and Fn reprsent the trampoline. it's how we bounce between
        */
      sealed abstract class Result {
        def get: M[B]
      }
      case class Complete(get: M[B]) extends Result
      case class ContResult[S, A](state: S, runFun: IFoo[S, M, A], fn: Fn[A]) extends Result {
        def get: M[B] =
          fn match {
            case ScalaFunc(sfn) => runFun.apply(state, sfn)
            // state is assumed pure here?
            case _ =>
              runFun.apply(state, { a =>
                eval(state, Pure[S, M, A](a), fn).get
              })
          }
      }

      // next action. this is where we push user methods from the heap onto the stack.
      sealed trait Fn[A]
      case class ScalaFunc[A](fn: A => M[B])                                 extends Fn[A]
      case class MappedFunc[A, C](fn: A => C, next: Fn[C])                   extends Fn[A]
      case class FlatMappedFunc[A, S, C](fn: A => Foo[S, M, C], next: Fn[C]) extends Fn[A]

      def eval[S, A](state: S, foo: Foo[S, M, A], fn: Fn[A]): Result =
        loop(state, foo.asInstanceOf[Foo[S, M, Any]], fn.asInstanceOf[Fn[Any]])

      @tailrec
      private def loop[S](state: S, foo: Foo[S, M, Any], fn: Fn[Any]): Result = foo match {
        case Pure(value) =>
          fn match {
            case ScalaFunc(sfn)        => Complete(sfn(value))
            case MappedFunc(mfn, next) => loop(state, Pure[S, M, Any](mfn(value)), next)
            case FlatMappedFunc(fmn, next) =>
              loop(state, fmn(value).asInstanceOf[Foo[S, M, Any]], next)
          }
        case Mapped(foo, mfn)     => loop(state, foo, MappedFunc(mfn, fn))
        case FlatMapped(foo, mfn) => loop(state, foo, FlatMappedFunc(mfn, fn))
        case F(f)                 => ContResult(state, f, fn)

      }
    }
    // END: RunEnv

  }

  implicit def fooFunctor[S, M[_]] = new Functor[Foo[S, M, ?]] {
    import Foo._
    def map[A, B](fa: Foo[S, M, A])(f: A => B): Foo[S, M, B] = Mapped[S, M, A, B](fa, f)
  }

  implicit def fooMonad[S, M[_]] = new Monad[Foo[S, M, ?]] {
    import Foo._

    def pure[A](a: A) = Pure(a)

    def flatMap[A, B](fa: Foo[S, M, A])(f: A => Foo[S, M, B]) = FlatMapped(fa, f)

    def tailRecM[A, B](a: A)(f: A => Foo[S, M, Either[A, B]]): Foo[S, M, B] = new Foo[S, M, B] {
      override def apply[X](state: S, cont: B => M[X]): M[X] =
        f(a).apply(state, {
          case Left(a) =>
            tailRecM(a)(f).apply(state, { b =>
              cont(b)
            })
          case Right(b) => cont(b)
        })
    }
  }

  def runFoo[S, M[_], A](foo: Foo[S, M, A], state: S)(implicit monad: Monad[M]): M[A] =
    foo.apply[A](state, { a =>
      monad.pure(a)
    })

  def stackTest(n: Int) = {
    import Foo._
    val monad                 = fooMonad[Unit, Id]
    val r: Foo[Unit, Id, Int] = monad.pure(333)

    val rr: Foo[Unit, Id, Int] = (1 to n).foldLeft(r)((r, _) => r.map(i => i + 1))

    runFoo[Unit, Id, Int](rr, Unit)
  }

}
