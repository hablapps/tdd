package tdd
package calculus

import org.scalatest._
import funspec._
import matchers._
import org.scalactic.Equality
import cats.Show
import cats.syntax.show.*
import tdd.util.*
import lambda.*

class ExamplesNegation[F: Form: Show, T: Term.Aux[F]: Equality: Show, C[_, _]: Calculus] extends AnyFunSpec with should.Matchers: 

    describe("Negations"): 

        it("False -> A"): 

            (Form[F].False implies "A".atom) should BeProvenBy(
                (0, Form[F].False).lam(0.`var`)
            )

        it("(A -> B) -> not(B) -> not(A)"): 

            (("A".atom implies "B".atom) implies ("B".atom.not implies "A".atom.not)) should BeProvenBy(
                (0, "A".atom implies "B".atom).lam(
                    (1, "B".atom implies Form[F].False).lam(
                        (2, "A".atom).lam(
                            1.`var`(0.`var`(2.`var`))
                        )
                    )
                )
            )

        ignore("not(not(A v not(A)))"):
            ("A".atom or "A".atom.not).not.not should BeProvenBy(
                (0, ("A".atom or "A".atom.not).not).lam(
                    0.`var`(
                        (1, "A".atom).lam(
                            0.`var`(1.`var`.inl("A".atom.not))
                        ).inr("A".atom)
                    )
                )
            )        