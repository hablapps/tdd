package tdd
package scala3

import scala.quoted.* 

type Env[Q <: Quotes] = (Q: Q) ?=> Map[Int, Q.reflect.Symbol]

object Env:

    def varName(i: Int): String = 
        s"x$i"

    def newVar(using Q: Quotes)(v: (Int, Q.reflect.TypeRepr)): Q.reflect.Symbol = 
        import quotes.reflect.* 
        
        Symbol.newVal(
            Symbol.spliceOwner,
            varName(v._1),
            v._2,
            Flags.EmptyFlags,
            Symbol.noSymbol)

    def substRefs(using Q: Quotes)(to: Q.reflect.Symbol, by: Q.reflect.Term)(term: Q.reflect.Term): Q.reflect.Term = 
        import Q.reflect.* 

        val subst = new TreeMap:
            override def transformTerm(tree: Term)(owner: Symbol): Term =
                tree match 
                    case Ident(name) if to.name == name => by
                    case e => super.transformTerm(e)(owner)
        
        subst.transformTerm(term)(Symbol.spliceOwner)
        