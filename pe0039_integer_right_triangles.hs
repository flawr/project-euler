--Problem 39
--If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
--{20,48,52}, {24,45,51}, {30,40,50}
--For which value of p = 1000, is the number of solutions maximised?

-- a = m^2 - n^2
-- b = 2mn
-- c = m^2 + n^2
-- perimeter a+b+c = 2m^2 + 2mn = 2m(m+n)
-- therefore perimeter = p = 2*m*(m+n) > 2*m*m

import Data.List

upperLimit = 1000
--(perimeter, sides) = (2*m*(m+n),[m^2-n^2,2*m*n, m^2+n^2])  
primitiveTriangles = sort [2*m*(m+n)|m<-[1.. upper],n<-[1..m-1],gcd m n == 1,odd (m-n) ] 
                     where upper = ceiling . sqrt $ fromIntegral(upperLimit `div`2)

toCounter [] = []
toCounter pts = (head l, length l):toCounter u
				   where l = takeWhile (==head pts) pts
				         u = dropWhile (==head pts) pts
						 
						 
countedPrimitiveTrianglesUnfiltered :: [(Integer, Int)]						 
countedPrimitiveTrianglesUnfiltered = toCounter primitiveTriangles

countedPrimitiveTriangles :: [(Integer, Int)]
countedPrimitiveTriangles = [(a,b) | (a,b)<-countedPrimitiveTrianglesUnfiltered,a<=upperLimit]

totalCount =  [(k,length[1|(a,b)<-countedPrimitiveTriangles,mod k a == 0]) |k<-[1..upperLimit]]

maxTuple [x] = x
maxTuple (x:xs) = if snd x > snd y then x else y 
                   where y = maxTuple xs
				
result = maxTuple totalCount
