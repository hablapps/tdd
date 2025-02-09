package tdd
package api

import org.scalatest._
import funspec._
import matchers._
import org.scalactic.Equality
import cats.Show
import tdd.api.{Form, Term}

class ExamplesImplies[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends AnyFunSpec with should.Matchers:
    
    describe("Implications"): 
        
        it("A -> A"): 
            ("A".atom implies "A".atom) should BeProvenBy(
                (0, "A".atom).lam(0.`var`)
            )

        it("((A → (B → R) -> R) → (A -> R) -> R) -> (A -> R) -> R"):

            ((("A".atom implies 
                (("B".atom implies "R".atom) implies "R".atom)) implies
                (("A".atom implies "R".atom) implies "R".atom)) implies 
                (("A".atom implies "R".atom) implies "R".atom)) should BeProvenBy(
                (0, ("A".atom implies (("B".atom implies "R".atom) implies "R".atom)) implies (("A".atom implies "R".atom) implies "R".atom)).lam(
                    (1, "A".atom implies "R".atom).lam(
                        0.`var`(
                            (2, "A".atom).lam(
                                (3, "B".atom implies "R".atom).lam(
                                    1.`var`(2.`var`)
                                )
                            )
                        )(1.`var`)
                    )
                ))

        it("A → B → A"):

            ("A".atom implies ("B".atom implies "A".atom)) should BeProvenBy(
                (0, "A".atom).lam(
                    (1, "B".atom).lam(
                        0.`var`))
            )

        it("(A → (B → C)) → ((A → B) → (A → C))"):

            (("A".atom implies ("B".atom implies "C".atom)) implies
                (("A".atom implies "B".atom) implies
                    ("A".atom implies "C".atom))) should BeProvenBy(
                (0, "A".atom implies ("B".atom implies "C".atom)).lam(
                    (1, "A".atom implies "B".atom).lam(
                        (2, "A".atom).lam(
                            0.`var`(2.`var`)(1.`var`(2.`var`))
                        )
                    )
                ))

    it("Fail: A → B → C"):

        ("A".atom implies ("B".atom implies "C".atom)) should Fail[F, T]

    it("Fail: (A → B) → C"):

        (("A".atom implies "B".atom) implies "C".atom) should Fail[F, T]
    