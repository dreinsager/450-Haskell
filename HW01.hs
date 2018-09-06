--CSCI 450--
--Homework 1--
--Doron Reinsager--


module HW01 (
  prodSqSmall, xor, implies, ccArea, validDay
)
where

  --Numer 2--
prodSqSmall :: (Double, Double, Double) -> Double
prodSqSmall (a, b, c)
  | a < b = prodSqSmall (b, a, c)
  | c < a = prodSqSmall (b, c, a)
  | otherwise = (a^2) * (b^2)

  --Number 3--
xor :: (Bool, Bool) -> Bool
xor (a,b)
  | a == b = False
  | otherwise = True

--Number 4--
implies :: (Bool, Bool) -> Bool
implies (p,q) =
  if p  == True && q == False
    then False
    else True

--Number 7--
ccArea :: (Double, Double) -> Double
ccArea (a,b) =
  if a > b
    then (pi * (a/2)^2) - (pi * (b/2)^2)
    else (pi * (b/2)^2) - (pi * (a/2)^2)
 

--Number 9--
addTax :: (Double, Double) -> Double
addTax (c,p) = 
  c + (c * (p * 0.01))

subTax :: (Double, Double) -> Double
subTax (c,p) = 
  c / (1 + p * 0.01)


--Number 11--
validDay :: (Int, Int, Int) -> Bool
validDay (m,d,y) =
  if y >= 0
    then
      if m >= 1 && m <= 12
        then
          if m == 4 || m == 6 || m == 9 || m == 11
            then
              if d >= 1 && d <= 30
                then True
                else False
           else if m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12
            then 
               if d >= 1 && d <= 31
                then True
                else False
            else if m == 2
              then 
                if y `mod` 4 == 0
                  then 
                    if d >=1 && d <= 29
                      then True
                      else False
                else if d >= 1 && d <= 28
                  then True
                  else False
            else False
        else False
    else False

--Number 12--
    
      


