import numpy as np
import matplotlib.pyplot as plt


# Lire les données du fichier
# Assurez-vous que les données sont séparées par des espaces
SPACE_200 = np.loadtxt('err.Nx.200.solv.2.dat')



# Extraire la troisième colonne
# value_H = []
# value_u = []
# value_tn = []


# Affichage
plt.figure()
plt.plot(SPACE_200[:,0], SPACE_200[:,1], '--b', linewidth=2, label='$h$')
plt.plot(SPACE_200[:,0], SPACE_200[:,3], ':r', linewidth=2, label='$u$')
plt.xlabel("$time$")
plt.ylabel("$L^1~error$")
# plt.xscale("log")
# plt.yscale("log")
plt.legend()
plt.title("Well-balanced verification (1st order)")
#plt.show()
plt.savefig("lake_at_rest_error_o1.png")