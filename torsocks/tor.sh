#!/bin/sh
nohup tor &
sleep 10
torsocks R -e "source('tw.R');get_tors()"
R -e "source('tw.R');translate_tors()"
