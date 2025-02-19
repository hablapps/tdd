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
import Form.Syntax.{given, *}
import Term.Syntax.{given, *}

class ExamplesDisjunction[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends AnyFunSpec with should.Matchers: 

    describe("Disjunctions"):
    
        it("A -> A v B"): 

            ("A" -> ("A" v "B")) should BeProvenBy(
                (0, "A".atom).lam(
                    0.inl("B")
                )
            )

        it("A v B -> B v A"):

            (("A" v "B") -> ("B" v "A")) should BeProvenBy(
                (0, "A" v "B").lam(
                    0.`match`(
                        (1, "A".atom).lam(1.inr("B".atom)), 
                        (2, "B".atom).lam(2.inl("A".atom))
                    )
                )
            )

        it("A & (B v C) -> A & B v A & C"):

            ("A" ^ ("B" v "C")) -> (("A" ^ "B") v ("A" ^ "C")) should
                BeProvenBy(
                    (0, "A" ^ ("B" v "C")).lam(
                        0._2.`match`(
                            (3, "B".atom).lam(
                                (0._1, 3).inl("A" ^ "C")
                            ),
                            (4, "C".atom).lam(
                                (0._1, 4).inr("A" ^ "B")
                            )
                        )
                    )
                )
            

