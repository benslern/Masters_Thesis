 Find best sigma at a resolution of 256 for alpha=16/1024, dt=2^-3 and T=5. Start from R128 optimized TGV at same sigma, refined to R256. Save eta at each iteration.

plot for [i=-1:-5:-1] "sigma_1E".i."/maximization_cost.dat" u 1:(-$2) w l title "sigm
a: ".i
