package tdd.util

import cats.data.* 
import cats.free.*
import cats.*
import cats.syntax.all.*

extension [A](l: List[A])
    def toNEL: Option[NonEmptyList[A]] = 
        l match 
            case Nil => None
            case h :: t => Some(NonEmptyList.of(h, t*))

given Cofree_Show[F[_]: Functor, A: Show](using alg: Algebra[F, Tree[Any]]): Show[Cofree[F, A]] with 

    val MAX_DEPTH = 3 

    def toTreeUntil(depth: Int)(cf: Cofree[F, A]): Tree[Any] = 
        Tree(cf.head.show, 
            if depth == 0 then Nil
            else List(alg(cf.tail.value.map(toTreeUntil(depth-1)))))

    def toTree(cf: Cofree[F, A]): Tree[Any] = 
        toTreeUntil(MAX_DEPTH)(cf)

    def show(t: Cofree[F, A]): String =
        toTree(t).show

