reset
iter=ARG1
resol=ARG2
scale=ARG3
set terminal png size 640,384
set output 'resol_'.resol.'/spectrum_'.iter.'.png'
set title "Spectrum - alpha=16/256, dt=2^{-5}, RESOL=".resol
unset key
set logscale y
set yrange [1E-70:1]
set xlabel "k"
set ylabel "e(k)"
set grid
plot for [i=0:32] "resol_".resol."/spectrum_fwd_".iter.".dat" every ::(i*scale)::((i+1)*scale)-2 u 1:2 w l title "iter: ".i

reset
set terminal png size 640,384
set output 'resol_'.resol.'/phi_vs_time.png'
set title "Phi vs Time - alpha=16/256, RESOL=".resol
set key bottom right
set xlabel "Time"
set ylabel "Phi"
set grid
plot "resol_".resol."/energy_fwd_".iter.".dat" u 1:6 w l title "dt: 2^{-5}"
