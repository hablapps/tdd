package tdd
package api

import org.scalatest._
import flatspec._
import matchers._
import org.scalactic.* 
import TripleEquals.*
import cats.Show, cats.syntax.show.*
import lj.SearchSpace.given
import lj.Proof.SearchStrategy
import tdd.api.{Form, Term}
import tdd.util.given

class BeProvenBy[F: Form: Show, T: Term.Aux[F]: Equality: Show](term: T, 
        through: SearchStrategy = lj.Proof.IterativeDeepening) extends Matcher[F]:

    def apply(formula: F) =
        val result: Option[T] = lj.Proof.programThrough[F, T](formula)(through)
        MatchResult(
            result.map(_ === term).getOrElse(false),
            s"Expected term ${term.show}, but " +
              result.fold(s"no proof was found in:\n${lj.SearchSpace.trace(formula).show}"): t => 
                s"found ${t.show}",
            s"Formula was proven correctly"
        )
