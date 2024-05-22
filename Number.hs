module Number(Number, createBasedNumber) where
import Text.XHtml (base)

newtype Base = Base {
    getBase :: Int
} deriving (Eq, Show)

data Number = Number Base [Int]

instance Eq Number where
  (==) :: Number -> Number -> Bool
  (==) a b = baseToDecimal a == baseToDecimal b

instance Ord Number where
  compare :: Number -> Number -> Ordering
  compare a b = compare (baseToDecimal a) (baseToDecimal b)

instance Num Number where
  (+) :: Number -> Number -> Number
  (+) le@(Number leftBase leftArr) ri@(Number rightBase rightArr) = decimalToBase (getBase leftBase) (baseToDecimal le + baseToDecimal ri)
  (*) :: Number -> Number -> Number
  (*) le@(Number leftBase leftArr) ri@(Number rightBase rightArr) = decimalToBase (getBase leftBase) (baseToDecimal le * baseToDecimal ri)
  abs :: Number -> Number
  abs num = num
  signum :: Number -> Number
  signum num = undefined
  fromInteger = undefined
  negate = undefined

instance Show Number where
  show :: Number -> String
  show (Number base arr) = "Base is: " ++ show (getBase base) ++ "\n" ++ "Number is: " ++  (unwords . map show $ arr)


decimalToBase :: Int -> Int -> Number
decimalToBase base n = convert base n
  where
    convert _ 0 = Number (Base base)  [0]
    convert b n = Number (Base base) $ reverse (convert' b n)
    convert' _ 0 = []
    convert' b n = let (q, r) = n `divMod` b in r : convert' b q

baseToDecimal :: Number -> Int
baseToDecimal (Number base arr) = foldl (\x y -> getBase base*x + y) 0 arr

createBasedNumber :: Int -> [Int] -> Number
createBasedNumber base arr =
    if any (\x -> x < 0 || x >= base) arr || null arr then error "Invalid number construction"
    else Number (Base base) (removeLeadingZeroes arr)
      where
      removeLeadingZeroes :: [Int] -> [Int]
      removeLeadingZeroes [0] = [0]
      removeLeadingZeroes (0:x) = removeLeadingZeroes x
      removeLeadingZeroes arr@(_:x) = arr
      removeLeadingZeroes _ = error "Illegal state"
