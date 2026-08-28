set term pngcairo nocrop enhanced font "verdana,14" size 1800, 1200
set output "plots/fpr-vs-bits.png"
set title "Bloom filter false positive rates (FPR) vs bits per entry" font ", 16"

set linetype 1 lc rgb "#32659c" lw 1 pt 1 ps 3
set linetype 2 lc rgb "#8bb4e1" lw 4 pt 1
set linetype 3 lc rgb "#32659c" lw 1 pt 2 ps 2
set linetype 4 lc rgb "#08274a" lw 3 pt 1
set linetype 5 lc rgb "red" pt 3 ps 3
set linetype 6 lc rgb "red" pt 3 ps 3


set xlabel "Bits per entry"
set xrange [1:25]
set grid xtics
set xtics 0,2,24
set grid ytics linewidth 2.5
set grid xtics linewidth 2.5

set ylabel "False Positive Rate (FPR), log scale"
set yrange [1e-5:1]
set logscale y
set format y "10^{%L}"
set grid ytics

plot "plots/fpr-vs-bits.classic.gnuplot.data" using 1 : 3 title "Classic, actual FPR" with points, \
     "plots/fpr-vs-bits.classic.gnuplot.data" using 1 : 2 title "Classic, calculated FPR" with lines, \
     "plots/fpr-vs-bits.blocked.gnuplot.data" using 1 : 3 title "Blocked, actual FPR" with points, \
     "plots/fpr-vs-bits.blocked.gnuplot.data" using 1 : 2 title "Blocked, calculated FPR" with lines, \
     "plots/fpr-vs-bits.original.gnuplot.data" using 1 : 3 title "Original, actual FPR" with points
