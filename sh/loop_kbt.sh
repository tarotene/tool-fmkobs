#!/usr/bin/env bash



while read line
do
  echo ${line} | ./Ising-2d/bin/Ising-2d
	# mkdir -p ./dat/snap/${line//\ /_} ./dat/stream/${line//\ /_}

  # find ./dat/snap/ -name \*.bin -type f -maxdepth 1 -print | xargs -I% mv % ./dat/snap/${line//\ /_}/
	# mv ./dat/snap/*.bin
  # find ./dat/stream/ -name \*.dat -type f -maxdepth 1 -print | xargs -I% mv % ./dat/stream/${line//\ /_}/
	# mv ./dat/stream/*.dat ./dat/stream/${line//\ /_}/
	echo "Done: ${line}."
done < ./dat/param/onlyvarkbt.dat
