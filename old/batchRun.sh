#!/bin/bash
killall aicup2020
./../launcher/aicup2020 --config ./config.json --batch-mode --save-results ./result.txt &
sleep 2

java -jar aicup2020-v8.jar  127.0.0.1 31001 &
java -jar aicup2020-v8.jar  127.0.0.1 31002 &
java -jar aicup2020-v8.jar  127.0.0.1 31003 &
java -jar aicup2020-v8.jar  127.0.0.1 31004 &
