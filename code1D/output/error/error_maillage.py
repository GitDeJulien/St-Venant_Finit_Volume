import numpy as np
import matplotlib.pyplot as plt


# Lire les données du fichier
# Assurez-vous que les données sont séparées par des espaces
SPACE_50 = np.loadtxt('err.Nx.40.solv.1.dat')
SPACE_100 = np.loadtxt('err.Nx.60.solv.1.dat')
SPACE_200 = np.loadtxt('err.Nx.80.solv.1.dat')
SPACE_400 = np.loadtxt('err.Nx.100.solv.1.dat')


SPACE_50_2 = np.loadtxt('err.Nx.40.solv.2.dat')
SPACE_100_2 = np.loadtxt('err.Nx.60.solv.2.dat')
SPACE_200_2 = np.loadtxt('err.Nx.80.solv.2.dat')
SPACE_400_2 = np.loadtxt('err.Nx.100.solv.2.dat')


SPACE_50_3 = np.loadtxt('err.Nx.40.solv.3.dat')
SPACE_100_3 = np.loadtxt('err.Nx.60.solv.3.dat')
SPACE_200_3 = np.loadtxt('err.Nx.80.solv.3.dat')
SPACE_400_3 = np.loadtxt('err.Nx.100.solv.3.dat')




# Extraire la troisième colonne
mean_value_1 = []
mean_value_2 = []
mean_value_3 = []

maillage = [40, 60, 80, 100]

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

coefs_1 = np.polyfit(np.log(maillage), np.log(mean_value_1), 1)
pred_f_1 = coefs_1[1] + np.log(maillage)*coefs_1[0]


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

coefs_2 = np.polyfit(np.log(maillage), np.log(mean_value_2), 1)
pred_f_2 = coefs_2[1] + np.log(maillage)*coefs_2[0]


mean_value_3.append((SPACE_50_3[-1,0]))
mean_value_3.append((SPACE_100_3[-1,0]))
mean_value_3.append((SPACE_200_3[-1,0]))
mean_value_3.append((SPACE_400_3[-1,0]))
# mean_value_2.append((SPACE_50_2[-1,1]))
# mean_value_2.append((SPACE_100_2[-1,1]))
# mean_value_2.append((SPACE_200_2[-1,1]))
# mean_value_2.append((SPACE_400_2[-1,1]))


mean_value_3 = np.array(mean_value_3)

coefs_3 = np.polyfit(np.log(maillage), np.log(mean_value_3), 1)
pred_f_3 = coefs_3[1] + np.log(maillage)*coefs_3[0]


# Affichage
plt.figure()
plt.plot(maillage, mean_value_1, '-ob', label='Rusanov (L1 error)')
plt.plot(maillage, mean_value_2 , '-^r', label='HLL (L1 error)')
plt.plot(maillage, mean_value_3 , '-Dg', label='VFRoe (L1 error)')
plt.plot(maillage, np.exp(pred_f_1), ':b', label=f'pente {-coefs_1[0]:.3f}')
plt.plot(maillage, np.exp(pred_f_2), ':r', label=f'pente {-coefs_2[0]:.3f}')
plt.plot(maillage, np.exp(pred_f_3), ':g', label=f'pente {-coefs_3[0]:.3f}')
plt.xlabel("$Nx$")
plt.xscale("log")
plt.yscale("log")
plt.xlim([maillage[0], maillage[-1]])
plt.legend()
#plt.show()
plt.savefig("space_error_L1_2D.png")