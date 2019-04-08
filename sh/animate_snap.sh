#!/usr/bin/env bash

echo -n "ilx = "
read ilx
echo -n "ily = "
read ily
echo -n "dkbt = "
read dkbt
echo -n "ivel = "
read ivel
echo -n "sxbc = "
read sxbc
echo -n "sybc = "
read sybc
echo -n "sfield = "
read sfield

rbx=$(echo "${ilx}+0.5" | bc)
rby=$(echo "${ily}+0.5" | bc)

lx=$(printf '%04g\n' ${ilx})
ly=$(printf '%04g\n' ${ily})
kbt=$(printf '%07.4f\n' ${dkbt})
vel=$(printf '%04g\n' ${ivel})
xbc="${sxbc}"
ybc="${sybc}"
field="${sfield}"

param="${lx}_${ly}_${kbt}_${vel}_${xbc}_${ybc}_${field}"

mkdir -p ./gif/${param}/
convert -delay 1 ./dat/snap/${param}/eq*.png ./gif/${param}/eq.gif
convert -delay 1 ./dat/snap/${param}/noneq*.png ./gif/${param}/noneq.gif
