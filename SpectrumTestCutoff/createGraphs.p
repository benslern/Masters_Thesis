getValue(row,col,filename) = system('awk ''{if (NR == '.row.') print $'.col.'}'' '.filename.'')


do for [k=0:5] {
  do for [i=10:10] {
    alpha=2**k

    reset
    set terminal png size 640,384
    set output 'alpha_'.alpha.'_256/resol_'.(2**i).'/spectrum_plot.png'
    set title "Spectrum - alpha=".alpha."/256, dt=2^{-5}, RESOL=".(2**i)
    unset key
    set logscale y
    set yrange [1E-70:1]
    set xlabel "|k|"
    set ylabel "Energy"
    set xrange [0:700*(2**(i-7))]
    set grid
    scale = 111*(2**(i-7))
    if(scale == 888){
      scale = 887
    }
    set arrow from scale*2*pi*2/3, graph 0 to scale*2*pi*2/3, graph 1 nohead lc rgb "red" lw 2
    plot for [j=0:100:10] "alpha_".alpha."_256/resol_".(2**i)."/spectrum_fwd_".alpha.".dat" every ::(j*scale)::((j+1)*scale)-1 u 1:2 w l title "resol: ".(2**i)
    
    reset
    set terminal png size 640,384
    set output 'alpha_'.alpha.'_256/resol_'.(2**i).'/enstrophy_plot.png'
    set title "Enstrophy vs Time - alpha=".alpha."/256, dt=2^{-5}, RESOL=".(2**i)
    set xlabel "Time"
    set grid
    set xrange [0:100]
    set key center right
    set key box
    plot "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:(($3*alpha*alpha)/65536) w l title "enstrophy" lt 1, "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:(($3*alpha*alpha)/65536) every 5::0 w p notitle lt 1, "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:($2) w l title "energy" lt 2, "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:($2) every 5::0 w p notitle lt 2, "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:($2 + ($3*alpha*alpha)/65536) w l title "alpha energy" lt 3, "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:($2 + ($3*alpha*alpha)/65536) every 5::0 w p notitle lt 3
    
    reset
    set terminal png size 640,384
    set output 'alpha_'.alpha.'_256/resol_'.(2**i).'/phi_plot.png'
    set title "Phi vs Time - alpha=".alpha."/256, dt=2^{-5}, RESOL=".(2**i)
    set xlabel "Time"
    set ylabel "Phi"
    set grid
    unset key
    set xrange [0:100]
    plot "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:6 w l
  
    reset
    set terminal png size 640,384
    set output 'alpha_'.alpha.'_256/resol_'.(2**i).'/epsilon_plot.png'
    set title "alpha energy error vs Time - alpha=".alpha."/256, dt=2^{-5}, RESOL=".(2**i)
    set xlabel "Time"
    set ylabel "epsilon"
    set grid
    unset key
    set xrange [0:100]
    set logscale y
    E0 = getValue(1,2,"alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat") + alpha*alpha*getValue(1,3,"alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat")/65536
    plot "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:(abs((($2 + ($3*alpha*alpha)/65536)-E0)/E0)) w l notitle lt 1
  }
}
