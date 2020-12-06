#!/bin/bash

#chmod +x runLoop1.sh
#chmod +x runLoop2.sh
#chmod +x runLoop3.sh

killAll() {
  kill 0
  exit
}

trap killAll SIGINT


./runLoop1.sh > /dev/null &
id1=$!
echo "runned $id1"
./runLoop2.sh > /dev/null &
id2=$!
echo "runned $id2"
./runLoop3.sh > /dev/null &
id3=$!
echo "runned $id3"

wait
echo "Finished"