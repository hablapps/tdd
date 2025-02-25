package tdd
package calculus

import org.scalatest._
import flatspec._
import matchers._
import org.scalactic.* 
import TripleEquals.*
import cats.Show, cats.syntax.show.*
import SearchSpace.given
import util.given
import lambda.* 

class BeProvenBy[F: Show, T: Term.Aux[F]: Equality: Show, C[_, _]](using Form[F], Calculus[C])(
    term: T, search: SearchStrategy = SearchStrategy.ID) 
extends Matcher[F]:

    import Calculus.given
    import SearchSpace.given

    def apply(formula: F) =
        val result: Option[T] = Proof.someProgram[F](formula)[T, C](search)
        MatchResult(
            result.map(_ === term).getOrElse(false),
            s"Expected term ${term.show}: ${formula.show}, but " +
              result.fold(s"no proof was found in:\n${SearchSpace.trace(formula).show}"): t => 
                s"found ${t.show} with proof: \n${Proof.someProof[F](formula)[C](search).get.show}",
            s"Formula was proven correctly"
        )
