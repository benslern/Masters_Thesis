reset
iter=ARG1
resol=ARG2
set terminal png size 640,384
set output 'spectrum_'.iter.'.png'
set title "Spectrum - alpha=8/256, dt=2^{".((-2*iter)-3)."}, RESOL=".resol
unset key
set logscale y
set yrange [1E-70:1]
set xlabel "k"
set ylabel "e(k)"
set grid
plot for [i=0:16] "spectrum_fwd_".iter.".dat" every ::(i*111)::((i+1)*111)-4 u 1:2 w l title "iter: ".i

reset
set terminal png size 640,384
set output 'phi_vs_time.png'
set title "Phi vs Time - alpha=8/256, RESOL=".resol
set key bottom right
set xlabel "Time"
set ylabel "Phi"
set grid
plot for [i=1:iter] "energy_fwd_".i.".dat" u 1:6 w l title "dt: 2^{".(-2*i-3)."}"
