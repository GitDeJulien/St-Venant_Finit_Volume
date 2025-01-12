# permet de définir les couleurs d'affichage
set palette defined ( 0 '#F7FBFF',\
                      1 '#DEEBF7',\
                      2 '#C6DBEF',\
                      3 '#9ECAE1',\
                      4 '#6BAED6',\
                      5 '#4292C6',\
                      6 '#2171B5',\
                      7 '#084594' )

# pour faire des png. Commenter pour un affichage classique.
#set terminal png

# forcer l'étendue de la colorbar. A changer selon les cas.
#set cbrange [0.0:1.2]

# force la mise à l'échelle des axes.
#set size ratio -1

# si affichage 3D, force l'étendue de l'axe des z. A changer selon les cas.
#set zrange [0.0:0.2]
set xrange [-2.0:2.0]
#set yrange [9.0:13.0]
set yrange [0.5:4.0]

set xlabel "x"
set ylabel "h(x,t)"
#set title  "First order wet dam-break"

# si affichage 3D, fixe le point de vue. A changer selon les cas.
#set view 100,100

#pour faire un gif
set term gif animate 
set output "sol.gif"

do for [i = 0:1000:5] {
    set title "iter = ".sprintf("%d", i)
    show title

    #-> 1D
    #plot "sol1D/z001/sol.".i.".dat" u 1:2 title "Z=0.01", "exact1D/sol.".i.".dat" u 1:2 lc 7 w l title "solution exact", "topo1D/topo.#".i.".dat" u 1:2 w l lc 8 title "topography", plot "sol1D/z01/sol.".i.".dat" u 1:2 title "Z=0.1", plot "sol1D/z10/sol.".i.".dat" #u 1:2 title "Z=0.1"
    plot "sol1D/test2/sol.".i.".dat" u 1:3 pt 3 lc 6 title "second order (HLL)", "topo1D/topo.".i.".dat" u 1:2 w l lc 8 title "topography"

    # fait un affichage 2D en couleur
    #plot "./sol.".i.".dat" u 1:2:3 palette with image
    ## fait un affichage en 3D et en couleur
    ## Pour le gif
    #splot "dat/sol.tps".i.".dat" u 1:2:3 title "solution approchee", "dat/exact/sol.tps".i.".dat" u 1:2:3 title "solution exact"
}

unset term
unset view