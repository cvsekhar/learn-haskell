--RWH CH3 Ex12
--http://en.wikipedia.org/wiki/Graham_scan
import Data.List

--Direction Type
data Direction = DLeft | DRight | DStraight deriving (Eq,Show)

-- 2D Point Type
data Point = Point Double Double deriving (Eq,Show)

-- cross product of two vectors
-- connecting (x1,y1) (x2,y2) and (x1,y1) (x3,y3)
cross :: Point -> Point -> Point -> Double
cross (Point x1 y1)(Point x2 y2)(Point x3 y3) = ((x2 - x1)*(y3-y1))-((y2-y1)*(x3-x1))

-- detemine the turn using cross product
-- of two vectors
turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
     | prod == 0 = DStraight
     | prod > 0 = DLeft
     | otherwise =DRight
    where prod = cross p1 p2 p3

-- computes direction from [a,b,c,d,e]
-- [a,b,c] then [b,c,d] and so on
dtriple :: [Point] -> [Direction]
dtriple (x:y:[]) = []
dtriple (x:y:z:xs) = (turn x y z) : (dtriple (y:z:xs))

-- find the point with the lowest y co ordinate
-- use compare fuction from haskell :)
lowestycoord :: Point -> [Point] -> Point
lowestycoord p [] = p
lowestycoord (Point a b) ((Point c d):xs)
        | b > d = lowestycoord (Point c d) xs
        | b < d = lowestycoord (Point a b) xs
        | b == d && a < c = lowestycoord (Point a b) xs
        | otherwise = lowestycoord (Point c d) xs

-- angle between two points with the x -axis
-- http://stackoverflow.com/questions/2676719/calculating-the-angle-between-the-line-defined-by-two-points
anglebtwnpoints :: Point -> Point -> Double
anglebtwnpoints (Point x1 y1)(Point x2 y2) = (atan2 ydiff xdiff) * rad2deg
                                             where rad2deg = 180.0/pi
                                                   ydiff = y2 - y1
                                                   xdiff = x2 - x1

-- http://www.fatvat.co.uk/2009_09_01_archive.html
-- use distance when anglebtwnpoints are equal
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)^2+(y1-y2)^2) 

-- compare points using anglebtwnpoints and distance 
comparepointsangle :: Point -> Point -> Point -> Ordering
comparepointsangle p a b = if angle == EQ then dist else angle
                           where angle = compare (anglebtwnpoints p a)(anglebtwnpoints p b)
                                 dist = compare (distance p a)(distance p b)

-- convert from [(x,y)] to [Point]
-- should have started with (x,y) started
-- doing exercise so added a wrapper
cxytop :: [(Double,Double)] -> [Point] -> [Point]
cxytop [] ys = ys
cxytop ((a,b):xs) ys = cxytop xs (Point a b : ys)

-- convert from [Point] to [(x,y)]
cptoxy :: [Point] -> [(Double,Double)] -> [(Double,Double)]
cptoxy [] ys = ys
cptoxy (Point a b : xs) ys = cptoxy xs ((a,b) : ys)

-- For each point, it is determined whether moving from the two
-- previously considered points to this point is a "left turn" 
-- or a "right turn". If it is a "right turn", this means that 
-- the second-to-last point is not part of the convex hull and 
-- should be removed from consideration (text from wikipedia)
scanremove :: [Point] -> [Point] -> [Point]
scanremove (x:y:[]) (ys) = x:y:ys
scanremove (x:y:z:xs) (ys) = if (turn x y z) == DRight 
                        then scanremove (x:z:xs) (ys)
                        else scanremove (y:z:xs) (x:ys)

-- graham scan algorithm
gscan :: [(Double,Double)] -> [(Double,Double)]
gscan xs = cptoxy (scanremove sl []) []
           where lc = lowestycoord (head cp) cp
                 sl = lc : sortBy (comparepointsangle lc) (delete lc cp)
                 cp = cxytop xs []

-- gscan [(0,0),(2,0),(0,2),(2,2),(1,1)]
-- gscan [(-3,1),(-4,1),(-1,4),(0,0),(2,2),(-1,3),(-1,2),(1,0),(3,-1),(-1,-1)] test case from ch3 RWH comments
