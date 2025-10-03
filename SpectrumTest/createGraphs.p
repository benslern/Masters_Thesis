do for [k=0:5] {
  do for [i=7:7] {
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
    set arrow from pi*(2**i), graph 0 to pi*(2**i), graph 1 nohead lc rgb "red" lw 2
    plot for [j=0:25] "alpha_".alpha."_256/resol_".(2**i)."/spectrum_fwd_".alpha.".dat" every ::(j*scale)::((j+1)*scale)-1 u 1:2 w l title "resol: ".(2**i)
    
    reset
    set terminal png size 640,384
    set output 'alpha_'.alpha.'_256/resol_'.(2**i).'/enstrophy_plot.png'
    set title "Enstrophy vs Time - alpha=".alpha."/256, dt=2^{-5}, RESOL=".(2**i)
    set xlabel "Time"
    set grid
    set xrange [0:100]
    plot "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:(($3*0.015625)) w l title "a*a*enstrophy", "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:($2) w l title "energy", "alpha_".alpha."_256/resol_".(2**i)."/energy_fwd_".alpha.".dat" u 1:($2+($3*0.015625)) w l title "sum"
    
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
  }
}
