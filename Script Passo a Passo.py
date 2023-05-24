# -*- coding: utf-8 -*-
"""
Análise Fatorial por Componentes Principais (PCA)

Wilson Tarantin Junior
Helder Prado Santos

"""

# Instalando e carregando os pacotes necessários

# Digitar o seguinte comando no console: pip install -r requirements.txt

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


#%% Criação do banco de dados com duas variáveis


df = pd.DataFrame({'var_1':[10,11,8,3,2,1],
                   'var_2':[6,4,5,3,2.8,1]})

df

#%% Gráfico com a dispersão de uma variável em um eixo

# Variável 1

plt.figure(figsize=(12,8))

plt.axhline(y=0, color='black', linewidth = 1, ls='--')
plt.scatter(df.var_1,[0] * df.shape[0], s=100)
plt.xlabel("Variável 1", fontsize=16)
plt.show()


#%% Gráfico com a dispersão de uma variável e função da outra


# Dispersão entre variável 1 e variável 2

plt.figure(figsize=(12,8))

plt.scatter(df.var_1, df.var_2, s=100)
plt.xlabel("Variável 1", fontsize=16)
plt.ylabel("Variável 2", fontsize=16)
plt.show()


#%% Encontrando as médias das variáveis

medias = df.T.mean(axis=1)

print(medias)

media_x, media_y = medias


#%% Encontrando as médias das variável 1 no gráfico


plt.figure(figsize=(12,8))

x = df.var_1.values
y = df.var_2.values

plt.scatter(x, y, s=200)

for i in range(len(x)):
    plt.plot([x[i],x[i]], [0,y[i]],'--', color='#2ecc71')
    
plt.scatter(media_x,0, marker="X", s=300, color='r')

plt.xlabel("Variável 1", fontsize=16)
plt.ylabel("Variável 2", fontsize=16)
plt.show()


#%% Encontrando as médias das variável 2 no gráfico


plt.figure(figsize=(12,8))

x = df.var_1.values
y = df.var_2.values

plt.scatter(x, y, s=200)

for i in range(len(y)):
    plt.plot([0,x[i]], [y[i],y[i]],'--', color='#2ecc71')
    
plt.scatter(0,media_y, marker="X", s=300, color='r')

plt.xlabel("Variável 1", fontsize=16)
plt.ylabel("Variável 2", fontsize=16)
plt.show()


#%% Obtendo novas coordenadas para transladar o eixo


# Novas coordenadas

x_trans = x - media_x

y_trans = y - media_y


#%% Transladar eixo do gráfico para o centro através das médias das observações


plt.figure(figsize=(12,8))

plt.scatter(x_trans, y_trans, s=200)
plt.axhline(y=0, color='black', ls='--')
plt.axvline(x=0, color='black', ls='--')
plt.xlabel("Variável 1", fontsize=16)
plt.ylabel("Variável 2", fontsize=16)
plt.show()


#%% Achar a melhor reta que sobrepõe todas observações através da origem 0,0

plt.figure(figsize=(12,8))

A = np.vstack([x_trans, np.ones(len(x_trans))]).T

m, c = np.linalg.lstsq(A, y_trans, rcond=None)[0]
m, c

x_fit = x_trans
pc1_fit = (m)*x_fit # ajustar a melhor curva

plt.scatter(x_trans, y_trans, s=200)
plt.axhline(y=0, color='black', ls='--')
plt.axvline(x=0, color='black', ls='--')
plt.plot(x_fit, pc1_fit, color='r', ls='--')
plt.ylim([-3,3])
plt.xlabel("Variável 1", fontsize=16)
plt.ylabel("Variável 2", fontsize=16)
plt.show()


#%% Obter o coeficiente angular da reta

coef_ang_pc1 = round(m,2)

print(coef_ang_pc1)


#%% Decompor o coeficiente para obter o vetor unitário através da hipotenusa

from fractions import Fraction

y_unit_pc1, x_unit_pc1 = Fraction(coef_ang_pc1).limit_denominator(1000).as_integer_ratio()

hipotenusa_pc1 = np.hypot(x_unit_pc1, y_unit_pc1)

print(x_unit_pc1, y_unit_pc1, hipotenusa_pc1)


#%% Encontrar autovetor da combinação linear do segundo componente (PC2)

# para var_1
x_unit_pc1/hipotenusa_pc1

# para var_1
y_unit_pc1/hipotenusa_pc1


#%% Printar o resultado dos autovetores do PC1

print(f"PC 1: mistura de {round(x_unit_pc1/hipotenusa_pc1,3)} partes de var_1 com {round(y_unit_pc1/hipotenusa_pc1,3)} de var_2")


#%% Retornar a reta perpendicular do component PC1 (sem nenhuma otimização)

plt.figure(figsize=(12,8))

A = np.vstack([x_trans, np.ones(len(x_trans))]).T

m, c = np.linalg.lstsq(A, y_trans, rcond=None)[0]
m, c

pc2_slope = -1/m

x_fit = x_trans
pc1_fit = m*x_fit
pc2_fit = pc2_slope*x_fit

plt.scatter(x_trans, y_trans, s=200)
plt.axhline(y=0, color='black', ls='--')
plt.axvline(x=0, color='black', ls='--')
    
plt.plot(x_fit, pc1_fit, color='r', ls='--')
plt.plot(x_fit, pc2_fit, color='b', ls='--')
plt.ylim([-3,3])
plt.xlabel("Variável 1", fontsize=16)
plt.ylabel("Variável 2", fontsize=16)
plt.show()


#%% Obter o coeficiente angular da reta

coef_ang_pc2 = round(pc2_slope,2)

print(coef_ang_pc2)


#%% Decompor o coeficiente para obter o vetor unitário através da hipotenusa

from fractions import Fraction

y_unit_pc2, x_unit_pc2 = Fraction(coef_ang_pc2).limit_denominator(1000).as_integer_ratio()

y_unit_pc2 *= -1 

x_unit_pc2 *= -1 

hipotenusa_pc2 = np.hypot(x_unit_pc2, y_unit_pc2)

print(x_unit_pc2, y_unit_pc2, hipotenusa_pc2)

#%% Encontrar autovetor da combinação linear do segundo componente (PC2)

# para var_1
x_unit_pc2/hipotenusa_pc2

# para var_2
y_unit_pc2/hipotenusa_pc2


#%% Printar o resultado dos autovetores do PC2

print(f"PC 2: mistura de {round(x_unit_pc2/hipotenusa_pc2,2)} partes de var_1 com {round(y_unit_pc2/hipotenusa_pc2,2)} de var_2")

#%% Material de referência:

# Prof. Joshua Starmer (Principal Component Analysis (PCA), Step-by-Step)