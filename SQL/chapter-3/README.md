# 第３章 集約と並び替え
## 集約関数
データの操作、計算には*関数*を使う。よく使うものは  
- COUNT テーブルのレコード数を数える
- SUM テーブルの数値列の合計
- AVG 同じく平均
- MAX テーブルの任意の列の最大値
- MIN 同じく最小値  

e.g.)  
~~~
SELECT COUNT(\*)
  FROM Shohin;

count
-----
  8
~~~  

NULLを除外するには具体的な列を指定する。  
~~~
SELECT COUNT(shiire_tanka)
  FROM Shohin;

count
-----
  6
(２行のNULLを含んでいたため)
~~~  

*MAX/MIN はどんなデータ型にも使えるが、SUM、AVGは無理*  

これらの関数はDISTINCTキーワードと組み合わせて重複を取り除いてから操作することもできる。  
~~~
SELECT COUNT (DISTINCT shohin_bunrui)
  FROM Shohin;


count
----
  3
~~~  

## テーブルをグループに切り分ける

GROUP BY句を使うとデータをグループに分けて集約できる。  
~~~
SELECT shohin_bunrui,COUNT(\*)
  FROM Shohin
GROUP BY shohin_bunrui;

shohin_bunrui  | count
衣服　　　　　 |  2
事務用品       |  2
キッチン用品   |  4
~~~  

GROUP BYに指定する列を集約キーやグループ化列と呼ぶ。  
書く順番はSELECT→FROM→WHERE→GROUP BYの順、ただしWHEREとGROUP BYの併用はWHEREが先に処理される。    
ちなみに集約キーがNULLの場合もちゃんと項目の１つとして処理される。  

### GROUP BYの鉄則

- SELECTに余計なもんを書いちゃいけない
  SELECTに書けるのは
  - 定数
  - 集約関数
  - 集約キー
  だけ。
- SELECTで付けた別名は使えない
  GROUP BYのほうが処理の順番が先だから処理できない
- 結果の順序はランダム、自動でソートされない
- WHERE句に集約関数を書かない
  COUNTの結果によって条件を分岐させたりしたいときにWHERE句にCOUNT(\*) = 2とか書きたいけどこれはダメ。
  集約関数はSELECT、HAVING句以外では使えない

## HAVING句

集約した結果を条件に指定したい場合はHAVING句を使う。  
~~~
SELECT shohin_bunrui,COUNT(\*)
  FROM Shohin
GROUP BY shohin_bunrui
  HAVING COUNT(\*) =2
~~~  

HAVING句でも  
- 定数
- 集約関数
- 集約キー
以外は書けないという制限がある。  

### WHERE句との使い分け

HAVING句でもWHERE句でも集約キーに関する条件は同じように書ける。結果は同じだが、WHERE句に書くほうがいいらしい。  
- HAVINGは*グループ*に対する条件指定で、*行*の条件指定はWHEREに書くべき
- WHERE句のほうが処理速度が速い
ってのが理由。  

## 検索結果の並べ替え

ORDER BYを使えば表示順の指定ができる、ちなみにこれを指定しないと順番は*全てランダム*になるので注意！  
~~~
SELECT ~
  FROM Shohin
ORDER BY hanbai_tanka;
~~~  
必ず*SELECT文の最後*に置く。デフォルトで昇順、降順にするには ``` ORDER BY hanbai_tanka DESC```のように書く。  
複数のソートキー指定も可能、左に書いたのが優先される。  
NULLは先頭か末尾にまとめられる、DBMSによっては指定できる。  
また、ORDER BYではSELECTで付けた別名も使用可能。  



