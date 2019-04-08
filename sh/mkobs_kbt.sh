#!/usr/bin/env bash

mkdir -p ./dat/obs 2>/dev/null

echo "# lx, ly, kbt, field, (m_total, err), (m_south, err), (m_north, err)" >./dat/obs/eq_m.dat
echo "# lx, ly, kbt, field, chi_total, chi_south, chi_north" >./dat/obs/eq_dm.dat
echo "# lx, ly, kbt, field, (e_total, err), (e_south, err), (e_north, err)" >./dat/obs/eq_e.dat
echo "# lx, ly, kbt, field, c_total, c_south, c_north" >./dat/obs/eq_de.dat
echo "# lx, ly, kbt, vel, field, (m_total, err), (m_south, err), (m_north, err)" >./dat/obs/noneq_m.dat
echo "# lx, ly, kbt, vel, field, chi_total, chi_south, chi_north" >./dat/obs/noneq_dm.dat
echo "# lx, ly, kbt, vel, field, (e_total, err), (e_south, err), (e_north, err)" >./dat/obs/noneq_e.dat
echo "# lx, ly, kbt, vel, field, c_total, c_south, c_north" >./dat/obs/noneq_de.dat
echo "# lx, ly, kbt, vel, field, (mb, err)" >./dat/obs/noneq_mb.dat
echo "# lx, ly, kbt, vel, field, (eb, err)" >./dat/obs/noneq_eb.dat
echo "# lx, ly, kbt, vel, field, (pw, err)" >./dat/obs/noneq_pw.dat

echo -n "[Fixed-parameter] dur_wolff = "
read dur_wolff
echo -n "[Fixed-parameter] dur_SSF = "
read dur_SSF
echo -n "[Fixed-parameter] isizeBin = "
read isizeBin

while read line
do
  echo "${line} ${dur_wolff} ${dur_SSF} ${isizeBin}" | ./bin/fmkobs
	echo "Done: ${line}."
done < ./dat/param/onlyvarkbt.dat
