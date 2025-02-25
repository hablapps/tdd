package tdd
package calculus

import org.scalatest._
import funspec._
import matchers._
import org.scalactic.Equality
import cats.Show, cats.syntax.show.*
import lambda.*
import Form.Syntax.{given, *}
import Term.Syntax.{given, *}

class ExamplesImplies[F: Form: Show, T: Term.Aux[F]: Equality: Show, C[_, _]: Calculus] extends AnyFunSpec with should.Matchers:
    
    describe("Implications"): 
        
        it("A -> A"): 
            ("A" -> "A") should BeProvenBy(
                (0, "A".atom).lam(0)
            )

        ignore("((A -> (B -> R) -> R) -> (A -> R) -> R) -> (A -> R) -> R"):

            ((("A" -> (("B" -> "R") -> "R")) -> (("A" -> "R") -> "R")) -> (("A" -> "R") -> "R")) should BeProvenBy(
                (0, ("A" -> (("B" -> "R") -> "R")) -> (("A" -> "R") -> "R")).lam(
                    (1, "A" -> "R").lam(
                        0(
                            (2, "A".atom).lam(
                                (3, "B" -> "R").lam(
                                    1(2)
                                )
                            )
                        )(1)
                    )
                ))

        it("A -> B -> A"):

            ("A" -> ("B" -> "A")) should BeProvenBy(
                (0, "A".atom).lam(
                    (1, "B".atom).lam(
                        0))
            )

        it("(A -> (B -> C)) -> ((A -> B) -> (A -> C))"):

            (("A" -> ("B" -> "C")) ->
                (("A" -> "B") ->
                    ("A" -> "C"))) should BeProvenBy(
                (0, "A" -> ("B" -> "C")).lam(
                    (1, "A" -> "B").lam(
                        (2, "A".atom).lam(
                            0(2)(1(2))
                        )
                    )
                ))

        it("Fail: A -> B -> C"):
            ("A" -> ("B" -> "C")) should Fail()

        it("Fail: (A -> B) -> C"):
            (("A" -> "B") -> "C") should Fail()
    
    describe("Implications with several proofs for the same lambda-term"): 
        
        it("(A -> B) -> A -> C -> B"): 
            (("A" -> "B") -> ("A" -> ("C" -> "B"))) should BeProvenBy(
                (0, "A" -> "B").lam(
                    (1, "A".atom).lam(
                        (2, "C".atom).lam(0(1))
                    )
                )
            )
            