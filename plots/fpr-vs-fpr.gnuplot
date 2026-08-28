set term pngcairo nocrop enhanced font "verdana,14" size 1800, 1200
set termoption font ", 14"
set output "plots/fpr-vs-fpr.png"
set title "Bloom filter actual false positive rate (FPR) vs requested FPR" font ", 16"
set key left top

set linetype 1 lc rgb "red" pt 3 ps 3
set linetype 2 lc rgb "#8bb4e1" lw 2 pt 2 ps 4
set linetype 3 lc rgb "#08274a" lw 2 pt 1 ps 3

set xlabel "Requested False Positive Rate (FPR), inverse log scale"
set xrange [1.1e-1:9e-5]
set logscale x
set format x "10^{%L}"
set grid xtics
set grid ytics linewidth 2.5
set grid xtics linewidth 2.5

set ylabel "Actual False Positive Rate (FPR), inverse log scale"
set yrange [1.2e-1:1e-7]
set logscale y
set format y "10^{%L}"
set grid ytics

plot "plots/fpr-vs-fpr.original.gnuplot.data" using 1 : 2 title "Original" with points, \
     "plots/fpr-vs-fpr.classic.gnuplot.data" using 1 : 2 title "Classic" with points, \
     "plots/fpr-vs-fpr.blocked.gnuplot.data" using 1 : 2 title "Blocked" with points

