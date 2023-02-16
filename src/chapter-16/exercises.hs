-- Exercise 1

add n (Succ m) = Succ (add n m)

Base case (n = Zero):

add Zero (Succ m)
=   { applying add }
Succ m
=   { unapplying add }
Succ (add Zero m)

Inductive case (n = Succ n):

add (Succ n) (Succ m)
=   { applying add }
Succ (add n (Succ m))
=   { inducative hypothesis }
Succ (Succ (add n m))
=   { unapplying add}
Succ (add (Succ n) m)


-- Exercise 2

 proerty to use: add n Zero = n, 
 
 show that addition is commutative, add n m = add m n, by induction on n.

Base case (n = Zero):

add Zero m
=   { applying add }
m 
=   { using property }
add m Zero

Inductive case (n = Succ n):

add (Succ n) m 
=   { applying add }
Succ (add n m)
=   { induction hypothesis }
Succ (add m n)
=   { using property from exercise 1 }
add m (Succ n)

