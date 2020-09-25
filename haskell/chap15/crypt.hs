data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
-- 暗号化関数
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2   -- アルファベットの中間値
        offset = fromEnum c + halfAlphabet    -- 中間値からオフセット
        rotation = offset `mod` alphabetSize  -- 剰余演算でEnumの範囲に収める

-- 文字を暗号化する関数
rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- 複数の要素を暗号化する場合
fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1, L3, L4, L1, L1, L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot41 vals
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot41 = rotN alphaSize

-- 奇数サイズのアルファベットの復号
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot31 vals
  where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
        rot31 = rotN alphaSize

-- 奇数、偶数に関わらず対応できるデコーダ
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN
        rotation = offset `mod` n

