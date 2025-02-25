package tdd
package calculus
package lj

import LJ.*
import cats.*, cats.syntax.all.*
import lambda.*

// For reuse in LJT
def traverse[F: Form, G[_]: Applicative, A, B](fa: Axiom[F, A])(f: A => G[B]): G[Axiom[F, B]] = 
    Axiom(fa.p, fa.gamma).pure

def traverse[F: Form, G[_]: Applicative, A, B](fa: LeftFalse[F, A])(f: A => G[B]): G[LeftFalse[F, B]] = 
    LeftFalse(fa.p, fa.gamma, fa.g).pure

def traverse[F: Form, G[_]: Applicative, A, B](fa: RightImplication[F, A])(f: A => G[B]): G[RightImplication[F, B]] = 
    f(fa.t).map(RightImplication(fa.p, fa.gamma, fa.b, _))

def traverse[F: Form, G[_]: Applicative, A, B](fa: LeftImplication[F, A])(f: A => G[B]): G[LeftImplication[F, B]] = 
    (f(fa.t1), f(fa.t2)).mapN(LeftImplication(fa.p, fa.gamma, fa.g, _, fa.x, _))
    
def traverse[F: Form, G[_]: Applicative, A, B](fa: RightConjunction[F, A])(f: A => G[B]): G[RightConjunction[F, B]] = 
    (f(fa.t1), f(fa.t2)).mapN(RightConjunction(fa.gamma, fa.a, fa.b, _, _))
    
def traverse[F: Form, G[_]: Applicative, A, B](fa: LeftConjunction[F, A])(f: A => G[B]): G[LeftConjunction[F, B]] = 
    f(fa.t).map(LeftConjunction(fa.p, fa.gamma, fa.g, fa.x, fa.y, _))
    
def traverse[F: Form, G[_]: Applicative, A, B](fa: RightDisjunctionL[F, A])(f: A => G[B]): G[RightDisjunctionL[F, B]] = 
    f(fa.t).map(RightDisjunctionL(fa.gamma, fa.a, fa.b, _))
    
def traverse[F: Form, G[_]: Applicative, A, B](fa: RightDisjunctionR[F, A])(f: A => G[B]): G[RightDisjunctionR[F, B]] = 
    f(fa.t).map(RightDisjunctionR(fa.gamma, fa.a, fa.b, _))
    
def traverse[F: Form, G[_]: Applicative, A, B](fa: LeftDisjunction[F, A])(f: A => G[B]): G[LeftDisjunction[F, B]] = 
    (f(fa.t1), f(fa.t2)).mapN(LeftDisjunction(fa.p, fa.gamma, fa.g, fa.x, _, fa.y, _))
        
        
