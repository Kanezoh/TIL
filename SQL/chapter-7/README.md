# 第7章　集合演算

## 7-1 テーブルの足し算と引き算

### 集合演算とは

テーブルやビュー、クエリの実行結果などのレコードの集合の足し算と引き算を行うこと。

#### テーブルの足し算　UNION

集合論の和集合と同じ操作を行う。
テーブルAが  
~~~  
shohin_id | shohin_mei
0001      | ラーメン
0002      | チャーハン
0003      | 餃子
~~~  
テーブルBが  
~~~
shohin_id | shohin_mei
0001      | ラーメン
0003      | 餃子
0004      | 麻婆豆腐
~~~  

これらをUNIONで結合すると  
~~~
SELECT shohin_id, shohin_mei
  FROM TableA
UNION
SELECT shohin_id, shohin_mei
  FROM TableB;

実行結果
shohin_id | shohin_mei
0001      | ラーメン
0002      | チャーハン
0003      | 餃子
0004      | 麻婆豆腐
~~~  

このように重複行は排除されて表示される。

**※注意**

- 演算対象のレコード数が一致していること
- 演算対象のレコードのデータ型が一致していること
- SELECT文では何を指定してもいいが、GROUP BYは最後に一つだけしか書けない

##### ALLオプション
以下のようにALLキーワードを付け加えると重複行を排除しなくなる。

~~~
SELECT shohin_id, shohin_mei
  FROM TableA
UNION ALL
SELECT shohin_id, shohin_mei
  FROM TableB;

実行結果
shohin_id | shohin_mei
0001      | ラーメン
0002      | チャーハン
0003      | 餃子
0001      | ラーメン
0003      | 餃子
0004      | 麻婆豆腐
~~~  

#### 共通部分の抽出　INTERSECT

集合論の積集合と同じ。  

~~~
SELECT shohin_id, shohin_mei
  FROM TableA
INTERSECT
SELECT shohin_id, shohin_mei
  FROM TableB;

実行結果
shohin_id | shohin_mei
0001      | ラーメン
0003      | 餃子
~~~  

これにもALLオプションがあり、重複行を排除しないようにすることもできる。

#### レコードの引き算  EXCEPT

集合の引き算を行う。
~~~
SELECT shohin_id, shohin_mei
  FROM TableA
UNION ALL
SELECT shohin_id, shohin_mei
  FROM TableB;

実行結果
shohin_id | shohin_mei
0002      | チャーハン
~~~  

ちなみに(2-4)と(4-2)の結果が違うように順番を入れ替えると結果も変わる。


## 7-2 結合

テーブルを行方向ではなく列方向に連結する演算。  

### 内部結合 - INNER JOIN

e.g.)   
~~~
SELCT TS.tenpo_id, TS.tenpo_mei, TS.shohin_id, S.shohin_mei, S.hanbai_tanka
  FROM TenpoShohin AS TS INNER JOIN Shohin AS S
    ON TS.shohin_id = S.shohin_id;
~~~  

FROM句に結合する複数のテーブルを記述、そしてONで結合に使用するキーを指定するのがポイント。  
テーブルを結合してしまえば、普通のテーブルのようにWHEREで条件を指定したり、ORDER BYで並べ替えたりすることが可能。  

### 外部結合 - OUTER JOIN

e.g.)   
~~~
SELCT TS.tenpo_id, TS.tenpo_mei, TS.shohin_id, S.shohin_mei, S.hanbai_tanka
  FROM TenpoShohin AS TS RIGHT OUTER JOIN Shohin AS S
    ON TS.shohin_id = S.shohin_id;
~~~  

外部結合では片方にしか存在しないデータでも必ず出力する。
RIGHT OUTER　か　LEFT OUTERを指定する。これはどちらのテーブルをマスタにするかを指定していて、
指定した方のデータは対応するもう片方のテーブルの値がなくても出力する。  

ちなみに、結合では3つ以上のテーブルも結合できる。その場合は2つのテーブルを結合した結果をもう一つのテーブルと結合して...のような流れになる。  

### クロス結合 - CROSS JOIN

実務ではまず使われることがない結合方法、2つのテーブルについて**全ての組み合わせ**を出力する。  

e.g.)  
~~~
SELECT TS.tenpo_id, TS.tenpo_mei, TS.shohin_id, S.shohin_mei
  FROM TenpoShohin AS TS CROSS JOIN Shohin AS S;
~~~  

実務で使われない理由
- 結果に使い道がない  
- 演算に多くの時間とマシンパワーを使用してしまう  

内部結合はクロス結合の組み合わせの一部であるという意味で「内部」、外部結合はクロス結合に存在しない組み合わせを持つという意味で「外部」である。
