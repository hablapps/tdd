package tdd
package calculus

import org.scalatest.*
import org.scalactic.*
import cats.Show
import lambda.*

class Suite[F: Form: Show, T: Term.Aux[F]: Equality: Show, C[_, _]: Calculus] extends Suites(
  ExamplesImplies[F, T, C],
  ExamplesConjunction[F, T, C],
  ExamplesDisjunction[F, T, C],
  ExamplesNegation[F, T, C]
)