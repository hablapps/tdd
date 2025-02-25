package tdd
package lambda

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

    object Atom: 
        def unapply[F: Form](f: F): Boolean = 
            f.fold[Boolean](
                false, 
                _ => true, 
                (_, _) => false,
                (_, _) => false,
                (_, _) => false)

    object False: 
        def unapply[F: Form](f: F): Boolean = 
            f.fold[Boolean](
                true, 
                _ => false, 
                (_, _) => false,
                (_, _) => false,
                (_, _) => false)

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

    object Syntax: 
        given [F: Form]: Conversion[String, F] = _.atom
        given [F: Form]: Conversion[F, F] = identity

        extension [F: Form, A](f: A)(using Conversion[A, F])
            def ->[B](f2: B)(using Conversion[B, F]): F = (f: F) implies f2
            def ^[B](f2: B)(using Conversion[B, F]): F = (f: F) and f2
            infix def v[B](f2: B)(using Conversion[B, F]): F = (f: F) or f2

