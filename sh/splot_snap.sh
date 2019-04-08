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
echo -n "nt_wolff2 = "
read nt_wolff2
echo -n "nt_SSF2 = "
read nt_SSF2

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

# mkdir -p ./png/${param}/

# TODO: parallelを使う
echo "${ilx} ${ily} ${nt_wolff2} ${nt_SSF2} ${dkbt} ${ivel} ${sxbc} ${sybc} ${sfield}" | ./bin2asc/bin/bin2asc
echo "Done: [binary => ascii]."

seq 1 1 ${nt_wolff2} | parallel --bar gnuplot -e \"t={}\;rbx=${rbx}\;rby=${rby}\;state=\'eq\'\;param=\'${param}\'\" ./gp/_splot_snap.gp
echo "Done: [ascii => image (eq)]."

seq 1 1 ${nt_SSF2} | parallel --bar gnuplot -e \"t={}\;rbx=${rbx}\;rby=${rby}\;state=\'noneq\'\;param=\'${param}\'\" ./gp/_splot_snap.gp
echo "Done: [ascii => image (noneq)]."

rm -f ./dat/snap/${param}/_*.dat
echo "Done: [Deleted ascii]."
