histogramFile="AP_Conservative_histogram.tsv"
historyFile="AP_Conservative_history.tsv"
set multiplot layout 2,1
set key top right
set ylabel "% in Bin"
set xlabel "Final Net Worth (in $1000s)"
set title "Final Net Worth: Histogram"
plot histogramFile u 1:($2*1.00) with boxes t ""
set key top left
set ylabel "USD (in 1000s)"
set xlabel "Year"
set title "Sample history from each quantile"
plot  historyFile u 1:($2/1000) with lines t "7%", historyFile u 1:($3/1000) with lines t "21%", historyFile u 1:($4/1000) with lines t "36%", historyFile u 1:($5/1000) with lines t "50%", historyFile u 1:($6/1000) with lines t "64%", historyFile u 1:($7/1000) with lines t "79%", historyFile u 1:($8/1000) with lines t "93%",

