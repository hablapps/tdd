package tdd
package api

trait Form[F]: 

    val False: F

    extension (p: String)
        def atom: F

    extension (a: F)
        def not: F = a implies False
        infix def implies(b: F): F
        infix def and(b: F): F
        infix def or(b: F): F 
        def fold[W](
            `false`: W,
            atom: String => W, 
            implies: (F, F) => W,
            and: (F, F) => W, 
            or: (F, F) => W): W

object Form: 

    def apply[F](using F: Form[F]): Form[F] = F

    object Implies: 
        def unapply[F: Form](f: F): Option[(F, F)] = 
            f.fold[Option[(F, F)]](
                None, 
                _ => None, 
                Some.apply(_, _),
                (_, _) => None,
                (_, _) => None)

    object And: 
        def unapply[F: Form](f: F): Option[(F, F)] = 
            f.fold[Option[(F, F)]](
                None, 
                _ => None, 
                (_, _) => None,
                (a, b) => Some((a, b)),
                (_, _) => None)

    object Or: 
        def unapply[F: Form](f: F): Option[(F, F)] = 
            f.fold[Option[(F, F)]](
                None, 
                _ => None, 
                (_, _) => None,
                (_, _) => None,
                (a, b) => Some((a, b)))
