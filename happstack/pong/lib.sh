benchmark(){
  sleep 5
  nx=$!
  httperf --hog --server=localhost --port=3000 --uri=/ --rate=1000 --num-conns=100 --num-calls=1000 --burst-length=20 > results
  kill $nx
  sleep 2
}
