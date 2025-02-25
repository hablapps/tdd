package tdd.lambda.scala3

import scala.quoted.* 

object Main extends App: 

    //derivation[((Int, Either[Char, Boolean])) => Either[(Int, Char), (Int, Boolean)]]
    //derivation[((Int, Char)) => Int]
    //derivation[((Int, Char)) => (Char, Int)]