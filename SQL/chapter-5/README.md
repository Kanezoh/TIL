# 第５章　ビュー

## ビューとは
テーブルと同じものだが、１つだけ「実際のデータを保存しているか否か」という点で違いがある。  
通常のテーブルではデータを記憶装置に保存しているが、ビューの場合はデータをどこにも保存していない。ビューは「SELECT文そのもの」を保存し、内部的にその文を実行、一時的に仮想のテーブルを作っている。  

### メリット
- 実際にデータを保存していないので記憶領域の節約になる。
- 頻繁に使うSELECT文をビューに保存しておけば使いまわせる。また、ビューは元のデータを参照しているため、元データが変更されても自動で返す内容も最新のものになる。  

### ビューの作り方

~~~
CREATE VIEW ビュー名 (<ビューの列名1>,<ビューの列名2>,...)
AS
<SELECT文>
~~~  
SELECT文の列とビューの列は並び順で一致する。  
e.g.)  
~~~
CREATE VIEW ShohinSum (shohin_bunrui, cnt_shohin)
AS
SELECT shohin_bunrui, COUNT(*)
  FROM Shohin
GROUP BY shohin_bunrui;
~~~  
実行するとビューが作成される、これはテーブルと同じように使える。  
e.g.)  
~~~
SELECT shohin_bunrui, cnt_shohin
  FROM ShohinSum;
~~~  
上の文はビューのSELECT文を実行した後に、さらにSELECT文が実行されるという構成になっている。  
ちなみに、ビューを元にさらにビューを定義することも可能だが、パフォーマンスが下がるのでやめたほうがいい。  

### ビューの制限事項

1. ビュー定義でORDER BYは使えない
   **行には順序がない**というルールからテーブルと同様にビューでもORDER BYは使えない。
2. ビューに対する更新
   テーブルと同様にビューでも更新系SQL文が実行できるが厳しい制限がつく。
   1. SELECT句にDISTINCTが含まれていない。
   2. FROM句に含まれるテーブルが１つだけである。
   3. GROUP BY句を使用していない。
   4. HAVING句を使用していない。

### ビューの削除

~~~
DROP VIEW <ビュー名>
~~~  

## サブクエリ
サブクエリ→**使い捨てのビュー**
ビューがSELECT文だけを保存してユーザの利便性を高める一方、サブクエリは**ビュー定義のSELECT文**をそのままFROM句に持ち込んだもの。  
~~~
商品分類ごとに総数を表示するビュー
CREATE VIEW ShohinSum (shohin_bunrui, cnt_shohin)
AS
SELECT shohin_bunrui, COUNT(*)
  FROM Shohin
GROUP BY shohin_bunrui;

ビューの確認
SELECT shohin_bunrui, cnt_shohin
  FROM ShohinSum;
~~~  

同じ事をサブクエリでやってみる。  
~~~
SELECT shohin_bunrui, cnt_shohin
  FROM (SELECT shohin_bunrui, COUNT(*) AS cnt_shohin
          FROM Shohin
        GROUP BY shohin_bunrui) AS ShohinSum;
~~~  
ShohinSumというのがこのサブクエリの名前だが、これはビューと違って保存はされない。名前の通りサブ(下位)のクエリ(問い合わせ)である。処理の流れ的には内側のSELECT文(サブクエリ)が実行されたのちに外側のクエリが実行される。  
また、サブクエリはネストすることもできるが、読みにくいから出来るだけやらないほうがいい。  

### スカラ・サブクエリ

**必ず１行1列だけの戻り値を返す**サブクエリのこと。  

#### 活用例：WHERE句で使う

「販売単価が全体の平均単価より高い商品だけを検索する」時にはどういう風に書くか。  
~~~
SELECT shohin_id, shohin_mei, hanbai_tanka
  FROM Shohin
WHERE hanbai_tanka > AVG(hanbai_tanka);
~~~  
それっぽいけど集約関数はWHERE句には使えない制限がある。  
そこでサブクエリの出番。まず、販売単価の平均を求めるクエリは  
~~~
SELECT AVG(hanbai_tanka)
  FROM Shohin;
~~~  
これを実行すると、  
~~~
          avg          
-----------------------
 2097.5000000000000000
(1 row)
~~~  
こんな感じになる。これは**スカラ値**(１行1列)であることは明白。  
このサブクエリを利用すれば、  
~~~
SELECT shohin_id, shohin_mei, hanbai_tanka
  FROM Shohin
WHERE hanbai_tanka > (SELECT AVG(hanbai_tanka)
                        FROM Shohin);
~~~  
最初の文はこのように書ける。サブクエリの部分は実行結果の2097.5に置き換わるため、有効な文となる。  
ちなみに、サブクエリはWHERE句に限らずほとんどどの句でも使える。  
ただ、**サブクエリが絶対に複数行を返さないようにする**ことだけは気をつけよう。  

## 相関サブクエリ

相関サブクエリは「商品分類ごとの平均販売単価を比較する」ような状況で使われる。先ほどの通常のサブクエリでは全体の平均単価を条件として比較することができたが、相関サブクエリでは条件を絞った状況下での条件比較を行うことができる。  
まずは商品分類ごとの平均価格を求める方法である。  
~~~
SELECT AVG(hanbai_tanka)
  FROM Shohin
GROUP BY shohin_bunrui;
~~~  
これをそのままWHEREに渡しても複数行の結果となるため実行できない。  
e.g.)  
~~~
SELECT shohin_id, shohin_mei, hanbai_tanka
  FROM Shohin
WHERE hanbai_tanka > (SELECT AVG(hanbai_tanka)
                        FROM Shohin
                      GROUP BY shohin_bunrui);
~~~  
ここで相関サブクエリが使える。１行追加するだけで求めたい結果が得られる。  
~~~
SELECT shohin_id, shohin_mei, hanbai_tanka
  FROM Shohin AS S1
WHERE hanbai_tanka > (SELECT AVG(hanbai_tanka)
                        FROM Shohin AS S2
                      WHERE S1.shohin_bunrui = S2.shohin_bunrui
                      GROUP BY shohin_bunrui);
~~~           
