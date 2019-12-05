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

