package tdd.api

import org.scalatest.*
import org.scalactic.*
import cats.Show
import tdd.api.{Form, Term}

class Suite[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends Suites(
  ExamplesImplies[F, T],
  ExamplesConjunction[F, T],
  ExamplesDisjunction[F, T],
  ExamplesNegation[F, T]
)