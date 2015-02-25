> module HaskellLab2
> 	where

> power :: Integer -> Integer -> Integer     
> power a 0 = 1     
> power a b = a * power a (b-1)

> powerT :: Integer -> Integer -> Integer
> powerT a b = trp b 1
>      where
>      trp n p = if (n==0) then p else trp (n-1) (a*p)

Problem 1
a) power 2 5
	a * power a (b-1)
	2 * power 2 (4)
	2 * (2 * power 2 (3))
	2 * (2 * (2 * power 2 (2)))
	2 * (2 * (2 * (2 * power 2 (1))))
	2 * (2 * (2 * (2 * (2 * power 2 (0)))))
	2 * (2 * (2 * (2 * (2 * 1))))
	2 * (2 * (2 * (2 * 2)))
	2 * (2 * (2 * 4))
	2 * (2 * 8)
	2 * 16
	32

b) (130025 reductions, 34115232 cells, 35 garbage collections)

c) powerT 2 5
	if (5==0) then p else trp (n-1) (a*p)
	if False then 2 else trp (4) (2*1)
	trp 4 2
	if (4==0) then p else trp (n-1) (a*p)
	if False then 2 else trp (3) (2*2)
	trp 3 4
	if (3==0) then p else trp (n-1) (a*p)
	if False then 4 else trp (2) (2*4)
	trp 2 8
	if (2==0) then p else trp (n-1) (a*p)
	if False then 8 else trp (2) (2*8)
	trp 1 16
	if (1==0) then p else trp (n-1) (a*p)
	if False then 16 else trp (0) (2*16)
	trp 0 32
	if (0==0) then p else trp (n-1) (a*p)
	if True then 32 else trp (-1) (2*32)
	32

d) (120026 reductions, 34105235 cells, 35 garbage collections)

e)
int powerT(int a, int b){
	int x = 1;
	while(b > 0){
		x = x * a;
		b--;
	}
	return x;
}

Problem 2

> turboPower a 0 = 1 
> turboPower a b 
>     | even b = turboPower (a*a) (b `div` 2) 
>     | otherwise = a * turboPower a (b-1)

a) This algorithm should execute in O(log n) where n is the power that you are taking the number to. Instead of iterating once for each power, the number is halved each time, reducing the number of loops or recursions greatly.

b) (1445 reductions, 92690 cells)

c)

> turboPower1 a b
>     | b == 0		= 1
>     | otherwise	= powerHelper a b 1

> powerHelper a b c
> 	| b == 0			= c
>	| even b 			= powerHelper (a*a) (b `div` 2) c
>	| otherwise			= powerHelper (a) (b-1) (c*a)

(1441 reductions, 92647 cells)

Problem 3

> sq [] = []
> sq xs = [y*y | y <- xs ]
 
Problem 4

> replicated [] = []
> replicated xs = [y| y <- xs , j <- [1 .. y]]

Problem 5

> ack m n
>	| m == 0			= n+1
>	| n == 0 && m > 0	= ack (m-1) 1
>	| n > 0 && m > 0	= ack (m-1) (ack m (n-1))

Problem 6

> data Point a = Pt a a deriving (Show, Eq)

> isOrigin (Pt a b)
>	| a == 0 && b == 0	= True
>	| otherwise			= False

b)

> isOriginL x = (\(Pt a b) -> if a == 0 && b == 0 then True else False) x 
	
