data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
-- 数字を暗号化する関数
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2   -- アルファベットの中間値
        offset = fromEnum c + halfAlphabet    -- 中間値からオフセット
        rotation = offset `mod` alphabetSize  -- 剰余演算でEnumの範囲に収める

-- 文字を暗号化する関数
rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- 複数の数字を暗号化する場合
fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1, L3, L4, L1, L1, L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot41 vals
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot41 = rotN alphaSize
