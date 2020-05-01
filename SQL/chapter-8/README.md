# 第8章 高度な処理

## GROUPING演算子

GROUP BYとSUMを使えば特定の集約キーにおける小計は出すことができるが、さらにその合計を表示することはできない。合計と小計を同時に表示するためにはGROUPING演算子を使う必要がある。  
- ROLLUP
- CUBE
- GROUPING SETS  
の3種類がある。  

### ROLLUP

~~~
SELECT shohin_bunrui, SUM(hanbai_tanka) AS sum_tanka
  FROM Shohin
GROUP BY ROLLUP(shohin_bunrui);
~~~  

実行結果  
~~~
shohin_bunrui    |  sum_tanka
                 |  16780
キッチン用品       |  11180
事務用品           |  600
衣服              |  5000
~~~  

この演算子では
1. GROUP BY()
2. GROUP BY(shohin_bunrui)  
が裏で実行されていて、1の何も指定していないGROUP BYが合計行のレコード(超集合行)となっている。  

