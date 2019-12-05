# jsで非同期通信

## 目次  

- コールバック関数とコールバック地獄、タイミング問題
- 解決策としてのPromise
- async/await Promiseをもっと使いやすく  


### コールバック関数とコールバック地獄、タイミング問題  

#### コールバック関数とは？
JavaScriptではブロック関数（処理が終わるまで待ち合わせる）より非同期関数（処理の完了を待たず、処理が完了した時点で
実行される関数）が多用される。
「別の関数に呼び出される関数」のこと、例えばsetTimeoutが一例  

~~~
setTimeout( () => {
  console.log('ahaha');
},2000)

2秒後に'ahaha'が出力される
~~~
setTimeoutでは第一引数として関数、第二引数として秒数（ミリ秒）を受け取り、秒数後に第一引数として渡された関数を実行する。  
する
この時、第一引数の関数は _コールバック関数_ と呼ばれる。ちなみに、setTimeoutのように関数を引数として受け取る関数は _高階関数_ と呼ばれる。  
前述の通り、非同期関数では処理の完了を待たないため、  
~~~
setTimeout( () => {
  console.log('ahaha');
},2000)
console.log('end');

結果：end
      200
~~~

のように書くと、setTimeoutの処理の完了を待たずにconsole.log('end')が評価される。



#### コールバック地獄とは？

非同期関数を使っていると、非同期関数で得た値を用いてまた非同期関数を呼び出したくなることがある。  
ここでは受け取った値を２秒後に２倍にして出力する関数twiceを例として使う。  

~~~
function twice(data,callback) {
  setTimeout( () => {
    callback(data*2);}
    ,2000);
}
twice(100,(value)=>{
  console.log(value);
})
=> 200
~~~
この関数を複数回呼び出してみるとこんなコードになる。  
~~~
twice(100,(value)=>{
  console.log(value);
  twice(value,(value)=>{
    console.log(value);
    twice(value,(value)=>{
      console.log(value);
    });
  });
})
=> 200
   400
   800
~~~

望む結果は得られたが、なんとも読みにくいコードになってしまった。JavaScriptには非同期関数が多く、想像よりもこのようなコードが
生まれる可能性が高い。この可読性の低いコールバックの連続を人々は憎しみを持って _コールバック地獄_ と呼んだ。  

#### タイミング問題とは？

コールバック地獄は可読性の低さだけが問題なのではなく、タイミングの問題も持つ。処理の完了を待たずに実行されるため、ブロック処理と違って処理の順番を把握することができないからだ。先ほどの例ではsetTimeoutを用いて２秒ごとに処理されるように
制御されていたが、console.log('end')の例を見てわかる通り、setTimeoutの外のことまでは段取りはつけられない。  

可読性が低く、処理の順番制御が困難なコード、これがコールバック地獄の本質である。


### 解決策としてのPromise

#### Promiseとは？

ES６で追加された非同期処理のための構文。百聞は一見に如かずということでまずはtwice関数を書き直す。  

~~~
function twice(data) {
  return new Promise( (resolve,reject) => {
    setTimeout( () =>{
      resolve(data*2);
    },2000);
  });
}


twice(100).then( (resolve) =>{
  console.log(resolve);
});
~~~

Promiseオブジェクトはthen(ok-callback,ng-callback)というメソッドを持ち、処理の成功、失敗が分かるまで処理を受け流す。
成功、失敗が分かったらコールバック関数を呼び出すという処理の流れになっている。  

この構文を用いると、コールバック地獄はこのように書き直すことができる。  

~~~
twice(100)
  .then( (resolve) =>{
    console.log(resolve);
    return twice(resolve);
  })
  .then( (resolve)=> {
    console.log(resolve);
    return twice(resolve);
  })
  .then( (resolve)=>{
    console.log(resolve);
  });
~~~

thenを使って次々に処理を繋いでいく。処理した後の値を引数としたtwice関数がreturnされており、これはPromiseオブジェクトを返すため、次のthenでまた評価されていくという仕組みだ。  

#### Promiseを使ったエラー処理

処理が失敗した時、つまりng-callbackが呼ばれる場合についても見てみる。  
次のコードはランダムで30%の確率で失敗になるようにしたtwice関数である。  
~~~
function twice(data){
  return new Promise((ok_callback,ng_callback)=>{
    setTimeout( ()=>{
      if (Math.random() < 0.30){
        ng_callback(new Error('error'));
      } else {
        ok_callback(data*2);
      }
    },2000);
  });
}


twice(100)
  .then( (resolve) =>{
    console.log(resolve);
  })
  .catch( (reject) => {
    console.log(reject);
  });
~~~

Promiseではエラーが発生すると最初にng-callback関数が指定されるまでthen処理をスキップする。  


#### async/await Promiseをもっと使いやすく

#### asyncとは

asyncはPromiseを返り値とする関数を簡単に定義できる構文である。returnした場合は戻り値はresolve,なんらかのエラーをthrowした
場合はrejectの結果を返す。  
例  

~~~
async function resolveSample(){
  return 'resolve';
}

resolveSample().then( (value) => {
  console.log(value);
});

async function rejectSample(){
  throw new Error('reject');
}

rejectSample().then ( (e) => {
  console.log(e);
});

~~~


#### awaitとは

async 関数内でPromiseの結果(resolve,reject)を返すまで処理を一時停止する演算子のこと。結果が返されたらasync関数の処理を再開。  
例：  

~~~
function sampleResolve(value) {
    return new Promise(resolve => {
        setTimeout(() => {
            resolve(value * 2);
        }, 2000);
    })
}

async function sample() {
    const result = await sampleResolve(5);
    return result + 5;
}

sample().then(result => {
    console.log(result); // => 15
});
~~~

非同期処理のタイミング問題を解決することができる。

また、先のthenをつないだコードももっと簡単に書くことができる。
~~~

function timeout(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function twice(value) {
  await timeout(2000);
  return value;
}

async function threetimes(data){
  const first=await twice(data);
  const second=await twice(first);
  const third= await twice(second);
  return first+second+third;
}

threetimes(100).then( (a) => {
  console.log(a);
});
~~~


