package tdd
package calculus

import util.*
import Calculus.given

import lambda.*
        
type SearchStrategy = [F, C[_, _]] => (Form[F], Calculus[C]) ?=> 
    SearchSpace[F, C] => LazyList[Proof[F, C]]

object SearchStrategy: 

    val DF: SearchStrategy = [F, C[_, _]] => (_, _) ?=> 
        Mu.sequenceF

    val ID: SearchStrategy = [F, C[_, _]] => (_, _) ?=> 
        
        val MAX = 10

        def go(depth: Int)(ss: SearchSpace[F, C]): LazyList[Proof[F, C]] =
            val results = Mu.sequenceF_Until(depth)(ss)
            if results != Nil then results
            else if depth > MAX then LazyList.empty
            else go(depth+1)(ss)
        
        go(1)
