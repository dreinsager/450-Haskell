--CSCI 450--
--Homework 2--
--References:
--https://www.tutorialspoint.com/haskell/
--Jason Dinh
--Doron Reinsager--

module WWPOP (
    formatDollars, formatLine, formatLines, calcSubtotal, formatAmt, formatBill, look, priceCart, makeBill,
)
where

import Text.Printf
type BarCode = Int
type Price = Int
type Name = String

type PriceList = [(BarCode,Name,Price)]

database :: PriceList
database = [ (1848, "Vanilla yogurt cups (4)", 188)]

type CartItems = [BarCode]
type CartPrices = [(Name,Price)]


type Bill = (CartPrices, Price, Price, Price)

taxRate :: Double
taxRate = 0.07


lineWidth :: Int
lineWidth = 34


--Number 1--
formatDollars :: Price -> String
formatDollars x = show(x `div` 100) ++ "." ++ printf "%02u"(x `mod` 100)

--Number 2--
formatLine :: (Name,Price) -> String
formatLine (name,price) = name ++ " " ++ (replicate lineWidth '.') ++ " " ++ formatDollars price ++ "\n"

--Number 3--
formatLines :: CartPrices -> String
formatLines [] = ""
formatLines cartPrices = (formatLine (head cartPrices)) ++ formatLines(tail cartPrices)

--Number 4--
calcSubtotal :: CartPrices -> Price
calcSubtotal [] = 0
calcSubtotal cartPrices = sum [x | (_,x) <- cartPrices]

--Number 5--
formatAmt :: String -> Price -> String
formatAmt name price = formatLine(name, price)

--Number 6--
formatBill :: Bill -> String
formatBill (w,x,y,z) = formatLines(w) ++ " " ++ show(x) ++ " " ++ show(y) ++ " " ++ show(z) ++ "\n"

--Number 7--
look :: PriceList -> BarCode -> (Name,Price)
look priceList barCode
 | null inside = ("None", 0)
 | otherwise = head inside
   where inside = [(n, p) | (b, n, p) <-priceList, b==barCode]

--Number 8--
priceCart :: PriceList -> CartItems -> CartPrices
priceCart database [] = []
priceCart database cartItems = look database (head cartItems) : priceCart database (tail cartItems)


--Number 9--
makeBill :: CartPrices -> Bill
makeBill cartPrices = (cartPrices, subtotal, taxtotal, total)
  where subtotal = calcSubtotal cartPrices
        taxtotal = round (taxRate * (fromIntegral (subtotal :: Int)))
        total = subtotal + taxtotal

--Number 10--
makeReceipt :: PriceList -> CartItems -> String
makeReceipt priceList cartItems = formatBill (makeBill (priceCart priceList cartItems))








 

