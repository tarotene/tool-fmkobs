#!/usr/bin/env bash

find ./dat/snap/ -name \*.dat -type f -print | xargs rm
find ./dat/snap/ -name \*.bin -type f -print | xargs rm
find ./dat/stream/ -name \*.dat -type f -print | xargs rm
find ./dat/stream/ -name \*.bin -type f -print | xargs rm
# find ./png/snap/ -name \*.png -type f -print | xargs rm
find ./gif/ -name \*.gif -type f -print | xargs rm

rm -r ./dat/snap/*
rm -r ./dat/stream/*
rm -r ./gif/*
