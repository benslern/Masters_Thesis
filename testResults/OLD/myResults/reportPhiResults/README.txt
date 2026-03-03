set title "Phi vs Tau - Projection and Retraction: (T=2^{[-2,1]})"
set xlabel "Tau"
set ylabel "Phi"
set grid
plot "report_cost_25.dat" u 1:2 w l title "T=0.25", "report_cost_50.dat" u 1:2 w l title "T=0.50", "report_cost_100.dat" u 1:2 w l title "T=1.00", "report_cost_200.dat" u 1:2 w l title "T=2.00"
