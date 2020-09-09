# ビット列: データをビットの集合で表現したデータ構造
# 「限られた範囲内にあり、密で、重複がなく、付随する情報もないデータのセット」を表すのに有効

# 問題
# 1. もしメモリが十分にあったならセットの表現やそのソート機能がライブラリにある言語を使って
# どのようにソートを実装するか考えよ。
arr = []
File.open('data.txt', 'r') do |f|
  f.each_line{|line| arr << line.chomp.to_i}
end
arr = arr.sort_by{ |n| n.to_i }
puts arr

# 2. ビットレベルの論理演算子を使ってビット列をどのように表現するか
arr = Array.new((1+10000000/32))

def set(i)
  arr[i >> 5] |= (1<<(i & 0x1f))
end

def clr(i)
  a[i >> 5] &= ()
end