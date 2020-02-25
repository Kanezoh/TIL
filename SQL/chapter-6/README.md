# 第６章　関数、述語、CASE式

## いろいろな関数

関数の種類  

- 算術関数
- 文字列関数
- 日付関数
- 変換関数
- 集約関数  

### 算術関数

ROUND関数などはNUMERIC型という特殊なデータ型でしか使えない。NUMERICで(全体の桁数、小数の桁数)で指定する。  
サンプル用データの作成  

~~~
CREATE TABLE SampleMath
(m NUMERIC (10,3),
 n INTEGER,
 p INTEGER);

 BEGIN TRANSACTION;

 INSERT INTO SampleMath(m, n, p) VALUES(500, 0, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(-180, 0, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(NULL, NULL, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(NULL, 7, 3);
 INSERT INTO SampleMath(m, n, p) VALUES(NULL, 5, 2);
 INSERT INTO SampleMath(m, n, p) VALUES(NULL, 4, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(8, NULL, 3);
 INSERT INTO SampleMath(m, n, p) VALUES(2.27, 1, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(5.555, 2, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(NULL, 1, NULL);
 INSERT INTO SampleMath(m, n, p) VALUES(8.76, NULL, NULL);

 COMMIT;
 ~~~

 #### ABS -絶対値

~~~
 SELECT m, ABS(m) AS abs_col
  FROM SampleMath;
=>
    m     | abs_col 
----------+---------
  500.000 | 500.000
 -180.000 | 180.000
          |        
          |        
          |        
          |        
    8.000 |   8.000
    2.270 |   2.270
    5.555 |   5.555
          |        
    8.760 |   8.760
~~~  

NULLの部分には何も入っていないが、ほとんどの関数はNULLに対してはNULLを返すようになっている。  

#### MOD -剰余

~~~
SELECT n,p, MOD(n,p) AS mod_col
  FROM SampleMath;

=>
n | p | mod_col 
---+---+---------
 0 |   |        
 0 |   |        
   |   |        
 7 | 3 |       1
 5 | 2 |       1
 4 |   |        
   | 3 |        
 1 |   |        
 2 |   |        
 1 |   |        
   |   |        
~~~  

**SQL SERVER**にはないので注意  

#### ROUND -四捨五入

```ROUND(対象数、丸めの桁数)```で指定する。  

~~~
SELECT m, n, ROUND(m,n) AS round_col
FROM SampleMath;
=>
    m     | n | round_col 
----------+---+-----------
  500.000 | 0 |       500
 -180.000 | 0 |      -180
          |   |          
          | 7 |          
          | 5 |          
          | 4 |          
    8.000 |   |          
    2.270 | 1 |       2.3
    5.555 | 2 |      5.56
          | 1 |          
    8.760 |   |          
~~~  

### 文字列関数

~~~
CREATE TABLE SampleStr
(str1 VARCHAR(40),
 str2 VARCHAR(40),
 str3 VARCHAR(40)
);

BEGIN TRANSACTION;

INSERT INTO SampleStr (str1,str2,str3) VALUES('あいう', 'えお', NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES('abc', 'def', NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES('山田', '太郎', 'です');
INSERT INTO SampleStr (str1,str2,str3) VALUES('aaa', NULL ,NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES(NULL, 'あああ', NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES('@!#$%', NULL, NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES('ABC', NULL, NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES('aBC', NULL, NULL);
INSERT INTO SampleStr (str1,str2,str3) VALUES('abc太郎', 'abc', 'ABC');
INSERT INTO SampleStr (str1,str2,str3) VALUES('abcdefabc', 'abc', 'ABC');
INSERT INTO SampleStr (str1,str2,str3) VALUES('ミックマック', 'ッ', 'っ');

COMMIT;

=>
     str1     |  str2  | str3 
--------------+--------+------
 あいう       | えお   | 
 abc          | def    | 
 山田         | 太郎   | です
 aaa          |        | 
              | あああ | 
 @!#$%        |        | 
 ABC          |        | 
 aBC          |        | 
 abc太郎      | abc    | ABC
 abcdefabc    | abc    | ABC
 ミックマック | ッ     | っ

~~~  

#### || -連結

~~~
SELECT str1, str2, str1 || str2 AS str_concat
  FROM SampleStr;

=>
     str1     |  str2  |   str_concat   
--------------+--------+----------------
 あいう       | えお   | あいうえお
 abc          | def    | abcdef
 山田         | 太郎   | 山田太郎
 aaa          |        | 
              | あああ | 
 @!#$%        |        | 
 ABC          |        | 
 aBC          |        | 
 abc太郎      | abc    | abc太郎abc
 abcdefabc    | abc    | abcdefabcabc
 ミックマック | ッ     | ミックマックッ
~~~  
3つ以上の文字列の連結も可能。  

#### LENGTH -文字列長
まあ分かると思うので省略
#### LOWER -小文字化
アルファベット以外には関係ない
#### UPPER -大文字化
#### REPLACE文字列の置換

```REPLACE(対象文字列、置換前の文字列、置換後の文字列)```  

~~~
SELECT str1, str2, str3, REPLACE(str1, str2, str3) AS rep_str
  FROM SampleStr;
=>
     str1     |  str2  | str3 |   rep_str    
--------------+--------+------+--------------
 あいう       | えお   |      | 
 abc          | def    |      | 
 山田         | 太郎   | です | 山田
 aaa          |        |      | 
              | あああ |      | 
 @!#$%        |        |      | 
 ABC          |        |      | 
 aBC          |        |      | 
 abc太郎      | abc    | ABC  | ABC太郎
 abcdefabc    | abc    | ABC  | ABCdefABC
 ミックマック | ッ     | っ   | ミっクマっク
~~~  

#### SUBSTRING -文字列の切り出し

```SUBSTRING(対象文字列 FROM 切り出し開始位置 FOR 切り出す文字数)```  

~~~
SELECT str1, SUBSTRING(str1 FROM 3 FOR 2) AS sub_str
  FROM SampleStr;
=>
     str1     | sub_str 
--------------+---------
 あいう       | う
 abc          | c
 山田         | 
 aaa          | a
              | 
 @!#$%        | #$
 ABC          | C
 aBC          | C
 abc太郎      | c太
 abcdefabc    | cd
 ミックマック | クマ
~~~  
標準SQLで認められている機能だが、**POSTGRESQLとMySQLのみ**でしか使えない。  







