package tdd
package api

import org.scalatest._
import funspec._
import matchers._
import org.scalactic.Equality
import cats.Show
import cats.syntax.show.*
import tdd.util.*
import tdd.api.{Form, Term}

class ExamplesDisjunction[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends AnyFunSpec with should.Matchers: 

    describe("Disjunctions"):
    
        it("A -> A v B"): 

            ("A".atom implies ("A".atom or "B".atom)) should BeProvenBy(
                (0, "A".atom).lam(
                    0.`var`.inl("B".atom)
                )
            )

        it("A v B -> B v A"):

            (("A".atom or "B".atom) implies ("B".atom or "A".atom)) should BeProvenBy(
                (0, "A".atom or "B".atom).lam(
                    0.`var`.`match`(
                        (1, "A".atom).lam(1.`var`.inr("B".atom)), 
                        (2, "B".atom).lam(2.`var`.inl("A".atom))
                    )
                )
            )

        it("A & (B v C) -> A & B v A & C"):

            ("A".atom and ("B".atom or "C".atom)) implies (("A".atom and "B".atom) or ("A".atom and "C".atom)) should
                BeProvenBy(
                    (0, "A".atom and ("B".atom or "C".atom)).lam(
                        0.`var`._2.`match`(
                            (3, "B".atom).lam(
                                (0.`var`._1 `and` 3.`var`).inl("A".atom and "C".atom)
                            ),
                            (4, "C".atom).lam(
                                (0.`var`._1 `and` 4.`var`).inr("A".atom and "B".atom)
                            )
                        )
                    )
                )
            

