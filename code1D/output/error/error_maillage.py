import numpy as np
import matplotlib.pyplot as plt


# Lire les données du fichier
# Assurez-vous que les données sont séparées par des espaces
SPACE_50 = np.loadtxt('err.Nx.50.solv.1.dat')
SPACE_100 = np.loadtxt('err.Nx.100.solv.1.dat')
SPACE_200 = np.loadtxt('err.Nx.200.solv.1.dat')
SPACE_400 = np.loadtxt('err.Nx.400.solv.1.dat')



SPACE_50_2 = np.loadtxt('err.Nx.50.solv.2.dat')
SPACE_100_2 = np.loadtxt('err.Nx.100.solv.2.dat')
SPACE_200_2 = np.loadtxt('err.Nx.200.solv.2.dat')
SPACE_400_2 = np.loadtxt('err.Nx.400.solv.2.dat')




# Extraire la troisième colonne
mean_value_1 = []
mean_value_2 = []
maillage = [50, 100, 200, 400]

# Calculer la moyenne

mean_value_1.append((SPACE_50[-1,0]))
mean_value_1.append((SPACE_100[-1,0]))
mean_value_1.append((SPACE_200[-1,0]))
mean_value_1.append((SPACE_400[-1,0]))
# mean_value_1.append((SPACE_50[-1,1]))
# mean_value_1.append((SPACE_100[-1,1]))
# mean_value_1.append((SPACE_200[-1,1]))
# mean_value_1.append((SPACE_400[-1,1]))



maillage = np.array(maillage)
mean_value_1 = np.array(mean_value_1)

coefs_1 = np.polyfit(np.log(1./maillage), np.log(mean_value_1), 1)
pred_f_1 = coefs_1[1] + np.log(1./maillage)*coefs_1[0]


# Calculer la moyenne

mean_value_2.append((SPACE_50_2[-1,0]))
mean_value_2.append((SPACE_100_2[-1,0]))
mean_value_2.append((SPACE_200_2[-1,0]))
mean_value_2.append((SPACE_400_2[-1,0]))
# mean_value_2.append((SPACE_50_2[-1,1]))
# mean_value_2.append((SPACE_100_2[-1,1]))
# mean_value_2.append((SPACE_200_2[-1,1]))
# mean_value_2.append((SPACE_400_2[-1,1]))


mean_value_2 = np.array(mean_value_2)

coefs_2 = np.polyfit(np.log(1./maillage), np.log(mean_value_2), 1)
pred_f_2 = coefs_2[1] + np.log(1./maillage)*coefs_2[0]

# Affichage
plt.figure()
plt.plot(1./maillage, mean_value_1, 'ob', label='Rusanov')
plt.plot(1./maillage, mean_value_2 , '^r', label='HLL')
# plt.plot(np.log(1./maillage), np.log(1./maillage), '--k', label="pente 1")
plt.plot(1./maillage, np.exp(pred_f_1), '--b', label=f'pente {coefs_1[0]}')
plt.plot(1./maillage, np.exp(pred_f_2), '--r', label=f'pente {coefs_2[0]}')
plt.title("Courbe d'erreur sur 1000 iterations")
plt.xlabel("$1/N_x$")
plt.ylabel("Erreur L1")
plt.xscale("log")
plt.yscale("log")
plt.xlim([1./maillage[0], 1./maillage[-1]])
plt.legend()
#plt.show()
plt.savefig("space_error_L1.png")