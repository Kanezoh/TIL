backwardsInfinity = reverse [1 .. ]

-- 6.5 練習問題
-- 6-1 ここで学んだ関数を使って指定された値を永遠に繰り返すrepeat関数を作ろう

repeat n = cycle [n]

-- 6-2 subseq関数を作ろう、開始位置、終了位置、リストを受け取ってその間にあるサブシーケンスを返す関数だ

subseq start end aList = take (end-start) (drop start aList)


-- 6-3 inFirstHalf関数を作ろう、この関数は要素がリストの前半分にあればTrue,それ以外ならFalseを返す。  

inFirstHalf element aList = elem element firstHalf
  where midpoint = (length aList) `div` 2
        firstHalf = take midpoint aList 
