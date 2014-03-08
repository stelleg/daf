#!/bin/bash
for file in `ls data/*.wav`; do
  ffmpeg -i $file ${file}.tmp.wav
  mv ${file}.tmp.wav ${file}
done
