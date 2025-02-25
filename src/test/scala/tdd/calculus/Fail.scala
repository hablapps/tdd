package tdd
package calculus

import org.scalatest._
import flatspec._
import matchers._
import org.scalactic.* 
import TripleEquals.*
import cats.Show, cats.syntax.show.*
import lambda.*
import tdd.util.given

class Fail[F: Form: Show, T: Term.Aux[F]: Equality: Show, C[_, _]: Calculus] extends Matcher[F]:

    def apply(formula: F) =
        val result: Option[T] = Proof.someProgram[F](formula)[T, C]()
        MatchResult(
            !result.isDefined,
            s"Expected no proof, but found term ${result.fold("should not happen")(_.show)}",
            s"Proof search failed, as expected"
        )
