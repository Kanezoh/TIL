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
