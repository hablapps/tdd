package tdd
package api
package lj

import cats.* 
import cats.syntax.all.*
import cats.arrow.*
import tdd.util.*
import SearchSpace.given 

type Proof[F] = Mu[[t] =>> CalculusF[F, t]]

object Proof: 

    def findDerivationRec[F: Form](find: Mu[SearchSpace.SearchF[F]] => Option[Proof[F]])(
            ss: Mu[SearchSpace.SearchF[F]]): Option[Proof[F]] = 
        ss.out()
            .iterator
            .map(_.traverse(find))
            .collect:
                case Some(f) => Mu(() => f)
            .nextOption

    // Plain DFS, not safe with infinite search spaces
    def findDerivation[F: Form](ss: Mu[SearchSpace.SearchF[F]]): Option[Proof[F]] = 
        findDerivationRec(findDerivation[F])(ss)

    // Iterative deepning with MAX depth: safe, but not complete
    def findDerivationWithID[F: Form](ss: Mu[SearchSpace.SearchF[F]]): Option[Proof[F]] = 

        val MAX = 10

        def findAtDepth(max: Int)(ss: Mu[SearchSpace.SearchF[F]]): Option[Proof[F]] = 
            if max == 0 then None
            else findDerivationRec(findAtDepth(max-1))(ss)

        def go(depth: Int)(ss: Mu[SearchSpace.SearchF[F]]): Option[Proof[F]] = 
            findAtDepth(depth)(ss).orElse(
                if depth < MAX then go(depth+1)(ss)
                else None)

        go(1)(ss)
            
    extension [F: Form: Show](form: F)
        
        def proof: Option[Proof[F]] = 
            val ss: SearchSpace[F] = Mu.unfold(SearchSpace.coalg)(api.Sequent.proof(form))
            findDerivationWithID(ss)
            
    extension [F: Form: Show, T: Term.Aux[F]](form: F)
        
        def program: Option[T] = 
            val derivation: Option[Proof[F]] = form.proof
            derivation.flatMap(Mu.fold(CalculusF.alg))
