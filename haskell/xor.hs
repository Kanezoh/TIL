xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

-- 数字をビット列に変換する
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder) == 0
               then False : intToBits' nextVal
               else True  : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal   = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

-- 桁を埋める
intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size-1,size-2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- ワンタイムパッド
myPad :: String
myPad = "Shhhhh"

myPlainText :: String
myPlainText = "Haskell"

-- 文字列とパッドをビットに変換
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair))
                            (zip padBits plaintextBits)
    where padBits = map charToBits pad
          plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad
