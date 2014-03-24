#!/bin/bash
for file in `ls -d $PWD/*.wav`; do
  echo "Converting $file"
  ffmpeg -i $file ${file}.tmp.wav
  mv ${file}.tmp.wav ${file}
done
