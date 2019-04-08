#!/usr/bin/env bash

:>./dat/param/onlyvarkbt.dat

scale=7
dscale=4

echo -n "[Fixed-parameter] ilx = "
read ilx
echo -n "[Fixed-parameter] ily = "
read ily
echo -n "[Fixed-parameter] nt_wolff0 = "
read nt_wolff0
echo -n "[Fixed-parameter] nt_wolff1 = "
read nt_wolff1
echo -n "[Fixed-parameter] nt_wolff2 = "
read nt_wolff2
echo -n "[Fixed-parameter] nt_SSF0 = "
read nt_SSF0
echo -n "[Fixed-parameter] nt_SSF1 = "
read nt_SSF1
echo -n "[Fixed-parameter] nt_SSF2 = "
read nt_SSF2
echo -n "[Fixed-parameter] ivel = "
read ivel
echo -n "[Fixed-parameter] sxbc = "
read sxbc
echo -n "[Fixed-parameter] sybc = "
read sybc
echo -n "[Fixed-parameter] sinitst = "
read sinitst
echo -n "[Fixed-parameter] sfield = "
read sfield
echo -n "[Variable-parameter] dkbt_min = "
read dkbt_min
echo -n "[Variable-parameter] dkbt_max = "
read dkbt_max
echo -n "[Variable-parameter] dkbt_stride = "
read dkbt_stride

for i in $(seq $(echo "scale=1; ${dkbt_min}*10^${dscale}" | bc) $(echo "scale=1; ${dkbt_stride}*10^${dscale}" | bc) $(echo "scale=1; ${dkbt_max}*10^${dscale}" | bc)); do
	echo "$(printf "%04d %04d %08d %08d %08d %08d %08d %08d %0${scale}.${dscale}f %08d\n" ${ilx} ${ily} ${nt_wolff0} ${nt_wolff1} ${nt_wolff2} ${nt_SSF0} ${nt_SSF1} ${nt_SSF2} $(echo "scale=${scale}; 0.1^${dscale}*${i}" | bc) ${ivel}) ${sxbc} ${sybc} ${sinitst} ${sfield}"
done > ./dat/param/onlyvarkbt.dat
