-- print list of optimal moves to solve tower of hanoi, exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- ps - peg source
-- pt - peg target
-- pm - peg middle
hanoi 1 ps pt _ = [(ps, pt)]
hanoi n ps pt pm = hanoi (n-1) ps pm pt  ++ [(ps, pt)] ++ hanoi (n-1) pm pt ps