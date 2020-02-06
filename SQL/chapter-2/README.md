# 第二章　検索の基本
## SELECT文の基本

テーブルからデータを取り出すときにSELECT文を使う、これを*問い合わせ*や*クエリ*と呼ぶ。  
~~~
SELECT <列名>,......
  FROM <TABLE名>;
~~~  

SELECT文はSELECT句、FROM句などの*句*からなる。句はSQL文を構成する要素でキーワードである。  
e.g.)  
~~~
SELECT shohin_id,shohin_mei,shiire_tanka
  FROM Shohin;

出力

shohin_id | shohin_mei | shiire_tanka
0001      | Tシャツ　　| 500
              .
              .
              .
              .  
~~~  
SELECT句の指定と同じ順番でデータが並ぶ。  
また、データを全部出すには\* を使う(並び順の指定は不可になる。)  
 
~~~
SELECT *
  FROM Sshohin;
~~~  

### 列に別名を付ける
ASキーワードで列に別名をつけられる。  
~~~
SELECT shohin_id AS id,
       shohin_mei AS namae,
       shiire_tanka AS tanka
  FROM Shohin;

出力
id   | namae  | tanka
0001 | Tシャツ| 500
.....................
~~~  
別名は日本語でもOKだが、その場合はダブルクォーテーションで囲う。  

### 定数の出力

~~~
SELECT '商品' AS mojiretsu, 38 AS kazu, '2009-09-24' AS hizuke,
       shohin_id,shohin_mei
  FROM Shohin;

出力
mojiretsu  | kazu | hizuke      |shohin_id   | shohin_mei|
商品       | 38   | 2009-09-24  | 0001       | Tシャツ
..........................................................
~~~  

### 結果から重複行を省く
~~~
SELECT DISTINCT shohin_bunrui
  FROM Shohin;

出力
shohin_bunrui 
キッチン用品
衣服
事務用品
~~~  

DISTINCTキーワードを使うと重複する項目は１つにまとめられる。また、NULLも１つの項目としてまとめられる。  
また、  
~~~
SELECT DISTINCT shohin_bunrui,torokubi
~~~  
のように指定すると、２つの要素がどちらとも重複している行のみが出力される。  

*DISTINCTキーワードは列の先頭にしか置けないから注意！*    

### WHERE句による行の選択

WHERE句に条件式を書けば、その条件を満たす行のみが出力される。  
e.g.)  

~~~
SELECT shohin_mei,shohin_bunrui
  FROM Shohin
WHERE shohin_bunrui = '衣服'
~~~  

*WHERE句はFROM句の直後に置く！！！*  

### コメント
一行  
~~~
-- 一行コメント
~~~  
複数行  
~~~
/\*
こんな感じで
書く
\*/
~~~  

### 算術演算子

~~~
SELECT shohin_mei,hanbai_tanka,
       hanbai_tanka * 2,hanbai_tanka_x2
  From Shohin;

shohin_mei| hanbai_tanka | hanbai_tanka_x2
Tシャツ   | 1000         | 2000
........
~~~  

+,-,\*,/ などの算術演算子はSELECT文で使える。  
*NULLを含んだ計算結果は問答無用でNULLになる*ので注意！  

### 比較演算子

~~~
SELECT shohin_mei,shohin_bunrui
  FROM Shohin
WHERE hanbai_tanka - shiire_tanka >= 500;
~~~  
のような感じで比較演算子を使って条件の指定ができる。  
- =  等しい
- <> 等しくない  
以上、以下は普通の言語と同じ。  

また、日付の場合は「より小さい」が「より前」になるので注意。  
また、文字列型の比較は「辞書式順序」で比較されるのでこれも注意、文字列型の数字とINTEGER型の数字ははっきりと区別される。  
NULLは比較演算子で使えない、<>演算子でも不可。NULLかどうかの判定には専用の*IS NULL,IS NOT NULL*演算子が用意されている。  

### 論理演算子
複数の条件を組み合わせることを可能にする演算子。  

否定の条件を表すNOT演算子、別に使わなくてもなんとかなることが多いので無理に使わないでいい  

~~~
WHERE NOT ~
~~~  

AND,OR演算子、条件を組み合わせられる。  
~~~
WHERE ~
  AND ~
  OR ~
~~~  
*AND,ORを併用する場合はORのほうが優先される*ので注意！()でくくればANDとORをうまく併用できる。  
AND,ORは真理値に基づいて条件を絞るが、比較演算子にNULLは使えないのと同じように、NULLでは真でも偽でもない*UNKNOWN*という第三の型になるので注意。  

