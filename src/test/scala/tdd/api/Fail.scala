package tdd
package api

import org.scalatest._
import flatspec._
import matchers._
import org.scalactic.* 
import TripleEquals.*
import cats.Show, cats.syntax.show.*
import lj.SearchSpace.given
import tdd.api.{Form, Term}
import tdd.util.given

class Fail[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends Matcher[F]:

    def apply(formula: F) =
        val result: Option[T] = lj.Proof.program[F, T](formula)()
        MatchResult(
            !result.isDefined,
            s"Expected no proof, but found term ${result.fold("should not happen")(_.show)}",
            s"Proof search failed, as expected"
        )
