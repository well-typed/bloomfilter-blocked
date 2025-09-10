set term png size 1800, 1200
set termoption font ", 14"
set output "plots/fpr-vs-fpr.png"
set title "Bloom filter actual false positive rate (FPR) vs requested FPR\nClassic and block-structured implementations" font ", 16"
# set subtitle "blah"
set key left top

set xlabel "Requested False Positive Rate (FPR), log scale"
set xrange [1.1e-1:9e-5]
set logscale x
set format x "10^{%L}"
set grid xtics

set ylabel "Actual False Positive Rate (FPR), log scale"
set yrange [1.2e-1:8e-5]
set logscale y
set format y "10^{%L}"
set grid ytics

plot "plots/fpr-vs-fpr.classic.gnuplot.data" using 1 : 2 title "Classic" with points pointtype 1 pointsize 2 linewidth 1.5 linecolor "blue", \
     "plots/fpr-vs-fpr.blocked.gnuplot.data" using 1 : 2 title "Blocked" with points pointtype 2 pointsize 2 linewidth 1.5 linecolor "red", \

