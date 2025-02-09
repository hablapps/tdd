package tdd.util

import cats.free.* 
import cats.*
import cats.syntax.all.*

case class Tree[+A](a: A, children: List[Tree[A]])

object Tree: 

    given [A]: Show[Tree[A]] with 

        def toString(t: Tree[A])(prefix: String, bar: String, sep: String): String = 
            prefix + sep + t.a.toString + 
            (t.children match 
                case List() => "" 
                case head :: tail => 
                    "\n" + 
                    (if tail.isEmpty then ""
                    else tail.map(toString(_)(prefix + bar + "   ", "|", "├──"))
                            .mkString("\n") + "\n") +
                    toString(head)(prefix + bar + "   ", "", "└──")       
            )

        def show(t: Tree[A]): String = 
            toString(t)("", "", "")
