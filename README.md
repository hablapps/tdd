# Automating Type-Driven Development with (Co)algebras

This repository serves as a testbed for exploring a coalgebraic approach to the implementation of sequent calculi. Currently, it includes only an implementation of the LJ and LJT calculi, but the same approach will eventually be applied to Linear Logic calculi, System L, etc. The implementation specifically uses Scala and Scala 3 macros.

## Design

### Calculus implementation

The implementation of a sequent-based calculus is carried out using a coalgebra and an algebra over the same characteristic functor. For instance, the following are some of the components of the LJ calculus functor, which is parameterized over a generic type F representing formulas:

```scala
enum LJ[F: Form, T]: 

    case Axiom(p: Prem[F], gamma: List[Prem[F]])(using Form[F])

    case LeftFalse(p: Int, gamma: List[Prem[F]], g: F)(using Form[F])

    case LeftImplication(p: Prem[F], gamma: List[Prem[F]], g: F, t1: T, x: Prem[F], t2: T)(using Form[F])

    case RightImplication(p: Prem[F], gamma: List[Prem[F]], b: F, t: T)(using Form[F])

    ...
```

This functor is a sum type with as many variants as there are rules defined in the calculus. For instance, let's consider the [left implication rule](https://www.sciencedirect.com/science/article/pii/030439759390181R) in the LJ calculus:


![Left-implication rule](left-impl-rule.png)

There are two key aspects to this rule: structuring the search process (bottom-up) and combining the proof terms (top-down). The algebra and coalgebra components of the Rule type class address both aspects:

```scala
given Rule[LeftImplication] with 

    def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication[F, Sequent[F]]] = 
        case seq@Sequent((p@(_, Implies(a, b))) :: gamma, g) =>
            val x = (seq.nextVar, b)
            LeftImplication(
                p, gamma, g, 
                Sequent(p :: gamma, a), 
                x, Sequent(x :: gamma, g))

    def alg[F: Form, T: Term.Aux[F]]: LeftImplication[F, T] => T = 
        case LeftImplication(p, _, _, t1, x, t2) => 
            t2.subst(x._1, p._1.`var`.apply(t1))
```

Altogether, these rules allow us to define the appropriate algebra and coalgebra for the LJ calculus as part of the corresponding instance of the `Calculus` type class:


```scala
given Calculus[LJ] with

    def coalg[F: Form]: Sequent[F] => LazyList[LJ[F, Sequent[F]]] = seq =>
        Rule[LJ.Axiom].coalg(seq.rotations) ++ 
        Rule[LJ.LeftFalse].coalg(seq.rotations) ++
        Rule[LJ.RightImplication].coalg(LazyList(seq)) ++ 
        ...

    def alg[F: Form, T](using Term[F, T]): LJ[F, T] => T = 
        case f: Axiom[F, T] => Rule[Axiom].alg(f)
        case f: LeftFalse[F, T] => Rule[LeftFalse].alg(f)
        case f: LeftImplication[F, T] => Rule[LeftImplication].alg(f)
        case f: RightImplication[F, T] => Rule[RightImplication].alg(f)
        ...
```

Note that the coalgebra is defined over the `[t] =>> LazyList[LJ[F, t]]` functor, whereas the algebra is defined over the `[t] =>> LJ[F, t]` functor. The coalgebra of the functor specifies how to unfold different types of sequents, while the algebra determines how to extract proof terms for a given sequent.

### Searching for proofs

Given this (co)algebraic formalization of the calculus, the search space for proofs is defined in terms of the final coalgebra of the functor `[t] =>> LazyList[LJ[F, t]]`, whereas proofs in the calculus are defined in terms of the final coalgebra of the functor `[t] =>> LJ[F, t]`. Technically, this should be the initial algebra, which differs from the final coalgebra in Scala, but we use the final coalgebra for convenience.

```scala
type SearchSpace[F, Calculus[_, _]] = Mu[[t] =>> LazyList[Calculus[F, t]]]

type Proof[F, Calculus[_, _]] = Mu[[t] =>> Calculus[F, t]]
```

To extract proofs from the search space, we need a search strategy:

```scala
type SearchStrategy = [F, C[_, _]] => (Form[F], Calculus[C]) ?=> 
    SearchSpace[F, C] => LazyList[Proof[F, C]]
```

Two strategies, depth-first and iterative deepening traversals are provided, implemented through a general sequence-like traversal of the final coalgebra:

``` scala
def sequenceF_Rec[G[_]: Monad, H[_]: Traverse](rec: Mu[G Compose H] => G[Mu[H]])(ss: Mu[G Compose H]): G[Mu[H]] = 
    Monad[G].flatMap(ss.out()): 
        _.traverse(rec)
            .map(Mu.in)

def sequenceF[G[_]: Monad, H[_]: Traverse](ss: Mu[G Compose H]): G[Mu[H]] = 
    sequenceF_Rec(sequenceF[G, H])(ss)

def sequenceF_Until[G[_]: Monad: Alternative, H[_]: Traverse](depth: Int)(ss: Mu[G Compose H]): G[Mu[H]] = 
    if depth == 0 then Alternative[G].empty
    else sequenceF_Rec(sequenceF_Until[G, H](depth-1))(ss)
```

Once we find a proof, we fold over it to obtain the corresponding proof term. All in all, the complete workflow for obtaining all proof terms for a given formula proceeds as follows:

```scala
def allPrograms[F: Form, T: Term.Aux[F], C[_, _]: Calculus](search: SearchStrategy): F => LazyList[T] = 
    Sequent. initial[F] andThen         // Sequent[F]
    Mu.unfold(Calculus.coalg) andThen   // Mu[[t] => LazyList[C[F, t]]]
    search[F, C] andThen                // LazyList[Proof[F]]
    (_.map(Mu.fold(Calculus.alg)))      // LazyList[T]
```

An algebra for the derived functor `[t] =>> LazyList[Calculus[F, t]]` can also be obtained from the calculus algebra, enabling the formulation of a brute-force algorithm for finite search spaces as a hylomorphism:

```scala
def alg[F: Form, T: Term.Aux[F], C[_, _]: Calculus]: Algebra[SearchF[F, C], LazyList[T]] = 
        _.flatMap(_.sequence[LazyList, T].map(Calculus.alg))

def allProofs[F: Form, T: Term.Aux[F], C[_, _]: Calculus](p: F): LazyList[T] = 
    Mu.hylo[[t] =>> LazyList[C[F, t]]](Calculus.coalg, alg)(Sequent.proof(p)) 
```

### Tagless-final encodings

Note that the declarations above are parameterized by the type classes Form and Term, which encode types and programs using the tagless-final approach. This enables a generic implementation of the search process that can be instantiated both for generating Scala 3 code and for an interpretation based on custom ADTs. Sample tests are written once and applied uniformly across different instantiations of the tagless-final APIs.

## Package structure

The implementation is structured into the following packages:

* `tdd`. Root package.
  * `tdd.calculus`. Declaration of common data types and type classes for encoding different sequent calculi.
    * `tdd.calculus.lj`. Implementation of the LJ sequent calculus.
    * `tdd.calculus.ljt`. Implementation of the LJT sequent calculus.
  * `tdd.lambda`. Declaration of type classes for terms and formulae of the untyped lambda calculus.
    * `tdd.lambda.initial`. Implementation of the lambda calculus in terms of a custom ADT.
    * `tdd.lambda.scala3`. Implementation in terms of Scala 3 AST.
  * `tdd.util`: Implementation of auxiliary types, not included in the Scala cats library.

Some tests are included under a similar package structure.

## Communication

Juan Manuel Serrano - <juanmanuel.serrano@urjc.es>

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.
