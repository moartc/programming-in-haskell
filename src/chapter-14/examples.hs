import Data.Monoid

t1 = mconcat [Sum 2, Sum 3, Sum 4]

t2 = mconcat [Product 2, Product 3, Product 4]

t3 = mconcat [All True, All True, All True]

t4 = mconcat [Any False, Any False, Any False]