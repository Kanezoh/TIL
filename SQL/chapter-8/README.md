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
                    16780
キッチン用品          11180
事務用品              600
衣服                 5000
~~~  

この演算子では
1. GROUP BY()
2. GROUP BY(shohin_bunrui)  
が裏で実行されていて、1の何も指定していないGROUP BYが合計行のレコード(超集合行)となっている。  

#### 集約キーを複数指定する場合

集約キーに"登録日"カラムを追加するとこんな感じになる。  
~~~  
shohin_bunrui   |  torokubi   |  sum_tanka
                                  20780 # 合計
キッチン用品                        8980 # 小計
キッチン用品       2008-04-09       880
キッチン用品       2009-04-09       6600
キッチン用品       2009-09-20       1500
事務用品                            5500 # 小計
事務用品           2010-11-15       2200
事務用品           2011-09-08       3300
衣服                               6300 # 小計
衣服              2012-03-15       2300
衣服                               4000
~~~  

商品分類と登録日をキーとして集約した結果の小計、合計が表示されている。  

### GROUPING演算子

上の例では衣服のtorokubiがNULLの場合はNULLが集約キーとなっていて、小計を表す超集合行と見分けがつきにくい。
そのため、超集合行のNULLを判別するためにGROUPING演算子が用意されている。  

~~~
SELECT GROUPING(shohin_bunrui), AS shohin_bunrui,
       GROUPING(torokubi) AS torokubi, SUM(hanbai_tanka) AS sum_tanka
FROM Shohin
GROUP BY( ROLLUP(shohin_bunrui, torokubi));
~~~  

~~~
shohin_bunrui      torokubi     sum_tanka
------------------------------------------
1                     1         16780
0                     1         11180
0                     0         880
0                     0         6800
0                     0         3500
0                     1         600
0                     0         500
0                     0         100
0                     1         5000  # 超集合行のNULLは1
0                     0         1000
0                     0         4000  # オリジナルデータのNULLは0
~~~  

これで超集合行のNULLとオリジナルデータのNULLを判別することができる。  
これを使えばCASE文などと合わせて表示をより分かりやすくできる。  
e.g.)
~~~
SELECT CASE WHEN GROUPING(shohin_bunrui) = 1,
            THEN '商品分類　合計'
            ELSE shohin_bunrui END AS shohin_bunrui,
      CASE WHEN GROUPING(torokubi) = 1,
            THEN '登録日　合計'
            ELSE torokubi END AS torokubi,
      SUM(hanbai_tanka) AS sum_tanka
FROM Shohin
GROUP BY ROLLUP(shohin_bunrui, torokubi);
~~~  


