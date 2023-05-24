# -*- coding: utf-8 -*-

#%% Análise Fatorial por Componentes Principais (PCA)

# Instalando os pacotes

# Digitar o seguinte comando no console: pip install -r requirements.txt

# Carregando os pacotes necessários

import pandas as pd
from factor_analyzer import FactorAnalyzer
from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity
from factor_analyzer.factor_analyzer import calculate_kmo
import pingouin as pg
import seaborn as sns
import numpy as np
import matplotlib.pyplot as plt

#%% Importando o banco de dados

# Fonte: Fávero e Belfiore (2017, Capítulo 10)

notas = pd.read_excel("Notas Fatorial.xlsx")

print(notas)

#%% Informações sobre as variáveis

print(notas.info())

print(notas.describe())

#%% Separando somente as variáveis quantitativas do banco de dados

notas_pca = notas[["finanças", "custos", "marketing", "atuária"]]

print(notas_pca)

#%% Matriz de correlaçãoes entre as variáveis

matriz_corr = pg.rcorr(notas_pca, method = 'pearson', upper = 'pval', decimals = 4, pval_stars = {0.01: '***', 0.05: '**', 0.10: '*'})
print(matriz_corr)

#%% Outra maneira de plotar as mesmas informações

corr = notas_pca.corr()

f, ax = plt.subplots(figsize=(11, 9))

mask = np.triu(np.ones_like(corr, dtype=bool))

cmap = sns.diverging_palette(230, 20, n=256, as_cmap=True)

sns.heatmap(notas.corr(), 
            mask=mask, 
            cmap=cmap, 
            vmax=1, 
            vmin = -.25,
            center=0,
            square=True, 
            linewidths=.5,
            annot = True,
            fmt='.3f', 
            annot_kws={'size': 16},
            cbar_kws={"shrink": .75})

plt.title('Matriz de correlação')
plt.tight_layout()
ax.tick_params(axis = 'x', labelsize = 14)
ax.tick_params(axis = 'y', labelsize = 14)
ax.set_ylim(len(corr))

plt.show()

#%% Teste de Bartlett

bartlett, p_value = calculate_bartlett_sphericity(notas_pca)

print(f'Bartlett statistic: {bartlett}')

print(f'p-value : {p_value}')


#%% Estatística KMO

kmo_all, kmo_model = calculate_kmo(notas_pca)

print(f'kmo_model : {kmo_model}')


#%% Definindo a PCA (procedimento preliminar)

fa = FactorAnalyzer()
fa.fit(notas_pca)


#%% Obtendo os Eigenvalues (autovalores)

ev, v = fa.get_eigenvalues()

print(ev)

# Temos 4 autovalores para este exemplo, pois são 4 variáveis

#%% Critério de Kaiser

# Verificar autovalores com valores maiores que 1
# Existem dois componentes acima de 1

#%% Parametrizando a PCA para dois fatores (autovalores > 1)

fa.set_params(n_factors = 2, method = 'principal', rotation = None)
fa.fit(notas_pca)


#%% Eigenvalues, variâncias e variâncias acumulada

eigen_fatores = fa.get_factor_variance()
eigen_fatores

tabela_eigen = pd.DataFrame(eigen_fatores)
tabela_eigen.columns = [f"Fator {i+1}" for i, v in enumerate(tabela_eigen.columns)]
tabela_eigen.index = ['Autovalor','Variância', 'Variância Acumulada']
tabela_eigen = tabela_eigen.T

print(tabela_eigen)

#%% Determinando as cargas fatoriais

cargas_fatores = fa.loadings_

tabela_cargas = pd.DataFrame(cargas_fatores)
tabela_cargas.columns = [f"Fator {i+1}" for i, v in enumerate(tabela_cargas.columns)]
tabela_cargas.index = notas_pca.columns
tabela_cargas

print(tabela_cargas)

#%% Determinando as comunalidades

comunalidades = fa.get_communalities()

tabela_comunalidades = pd.DataFrame(comunalidades)
tabela_comunalidades.columns = ['Comunalidades']
tabela_comunalidades.index = notas_pca.columns
tabela_comunalidades

print(tabela_comunalidades)

#%% Resultados dos fatores para as observações do dataset (predict)

predict_fatores= pd.DataFrame(fa.transform(notas_pca))
predict_fatores.columns =  [f"Fator {i+1}" for i, v in enumerate(predict_fatores.columns)]

print(predict_fatores)

# Adicionando ao dataset 

notas = pd.concat([notas.reset_index(drop=True), predict_fatores], axis=1)

notas

#%% Identificando os scores fatoriais

scores = fa.weights_

tabela_scores = pd.DataFrame(scores)
tabela_scores.columns = [f"Fator {i+1}" for i, v in enumerate(tabela_scores.columns)]
tabela_scores.index = notas_pca.columns
tabela_scores

print(tabela_scores)

#%% Correlação entre os fatores

# A seguir, verifica-se que a correlação entre os fatores é zero (ortogonais)

corr_fator = pg.rcorr(notas[['Fator 1','Fator 2']], method = 'pearson', upper = 'pval', decimals = 4, pval_stars = {0.01: '***', 0.05: '**', 0.10: '*'})
print(corr_fator)

#%% Criando um ranking

notas['Ranking'] = 0

for index, item in enumerate(list(tabela_eigen.index)):
    variancia = tabela_eigen.loc[item]['Variância']

    notas['Ranking'] = notas['Ranking'] + notas[tabela_eigen.index[index]]*variancia
    
print(notas)

#%% Gráfico das cargas fatoriais e suas variâncias nos componentes principais

import matplotlib.pyplot as plt

plt.figure(figsize=(12,8))

tabela_cargas_chart = tabela_cargas.reset_index()

plt.scatter(tabela_cargas_chart['Fator 1'], tabela_cargas_chart['Fator 2'], s=30)

def label_point(x, y, val, ax):
    a = pd.concat({'x': x, 'y': y, 'val': val}, axis=1)
    for i, point in a.iterrows():
        ax.text(point['x'] + 0.05, point['y'], point['val'])

label_point(x = tabela_cargas_chart['Fator 1'],
            y = tabela_cargas_chart['Fator 2'],
            val = tabela_cargas_chart['index'],
            ax = plt.gca()) 

plt.axhline(y=0, color='black', ls='--')
plt.axvline(x=0, color='black', ls='--')
plt.ylim([-1.5,1.5])
plt.xlim([-1.5,1.5])
plt.title(f"{tabela_eigen.shape[0]} componentes principais que explicam {round(tabela_eigen['Variância'].sum()*100,2)}% da variância", fontsize=14)
plt.xlabel(f"PC 1: {round(tabela_eigen.iloc[0]['Variância']*100,2)}% de variância explicada", fontsize=14)
plt.ylabel(f"PC 2: {round(tabela_eigen.iloc[1]['Variância']*100,2)}% de variância explicada", fontsize=14)
plt.show()


#%% Gráfico da variância acumulada dos componentes principais

plt.figure(figsize=(12,8))

plt.title(f"{tabela_eigen.shape[0]} componentes principais que explicam {round(tabela_eigen['Variância'].sum()*100,2)}% da variância", fontsize=14)
sns.barplot(x=tabela_eigen.index, y=tabela_eigen['Variância'], data=tabela_eigen, color='green')
plt.xlabel("Componentes principais", fontsize=14)
plt.ylabel("Porcentagem de variância explicada", fontsize=14)
plt.show()

#%% Exercício 2

# Importando o banco de dados
# Fonte: Fávero e Belfiore (2017, Capítulo 10)

paises = pd.read_excel("Indicador País (PCA).xlsx")

print(paises.head())


#%% Informações do dataset

print(paises.info())

print(paises.describe())

#%% Selecionando as variáveis

paises_pca = paises.drop(columns=['país', 'cpi2', 'violência2', 'pib_capita2', 'escol2'])

# Vamos fazer apenas para o ano 1

#%% Matriz de correlaçãoes entre as variáveis

matriz_corr = pg.rcorr(paises_pca, method = 'pearson', upper = 'pval', decimals = 4, pval_stars = {0.01: '***', 0.05: '**', 0.10: '*'})
print(matriz_corr)

#%% Graficamente

# Outra maneira de plotar as mesmas informações

corr = paises_pca.corr()

f, ax = plt.subplots(figsize=(11, 9))

mask = np.triu(np.ones_like(corr, dtype=bool))

cmap = sns.diverging_palette(230, 20, n=256, as_cmap=True)

sns.heatmap(corr, 
            mask=mask, 
            cmap=cmap, 
            vmax=1, 
            vmin = -.25,
            center=0,
            square=True, 
            linewidths=.5,
            annot = True,
            fmt='.3f', 
            annot_kws={'size': 16},
            cbar_kws={"shrink": .75})

plt.title('Matriz de correlação')
plt.tight_layout()
ax.tick_params(axis = 'x', labelsize = 12)
ax.tick_params(axis = 'y', labelsize = 12)
ax.set_ylim(len(corr))

plt.show()

#%% Teste de Bartlett

bartlett, p_value = calculate_bartlett_sphericity(paises_pca)

print(f'Bartlett statistic: {bartlett}')

print(f'p-value : {p_value}')


#%% Estatística KMO

kmo_all, kmo_model = calculate_kmo(paises_pca)

print(f'kmo_model : {kmo_model}')

#%% Definindo a PCA (inicial)

fa = FactorAnalyzer()
fa.fit(paises_pca)

#%% Obtendo os Eigenvalues

ev, v = fa.get_eigenvalues()

print(ev)

#%% Critério de Kaiser

## Verificar eigenvalues com valores maiores que 1

print([item for item in ev if item > 1])

#%% Parametrizando a PCA para 1 fator (autovalores > 1)

fa.set_params(n_factors = 1, method = 'principal', rotation = None)
fa.fit(paises_pca)

#%% Eigenvalues, variâncias e variâncias acumulada

eigen_fatores = fa.get_factor_variance()
eigen_fatores

tabela_eigen = pd.DataFrame(eigen_fatores)
tabela_eigen.columns = [f"Fator {i+1}" for i, v in enumerate(tabela_eigen.columns)]
tabela_eigen.index = ['Autovalor','Variância', 'Variância Acumulada']
tabela_eigen = tabela_eigen.T

print(tabela_eigen)

#%% Determinando as cargas fatoriais

cargas_fatores = fa.loadings_

tabela_cargas = pd.DataFrame(cargas_fatores)
tabela_cargas.columns = [f"Fator {i+1}" for i, v in enumerate(tabela_cargas.columns)]
tabela_cargas.index = paises_pca.columns
tabela_cargas

print(tabela_cargas)

#%% Determinando as comunalidades

comunalidades = fa.get_communalities()

tabela_comunalidades = pd.DataFrame(comunalidades)
tabela_comunalidades.columns = ['Comunalidades']
tabela_comunalidades.index = paises_pca.columns
tabela_comunalidades

print(tabela_comunalidades)

#%% Resultado do fator para as observações do dataset

predict_fatores= pd.DataFrame(fa.transform(paises_pca))
predict_fatores.columns =  [f"Fator {i+1}" for i, v in enumerate(predict_fatores.columns)]

# Adicionando ao banco de dados

paises = pd.concat([paises.reset_index(drop=True), predict_fatores], axis=1)

#%% Identificando os scores fatoriais

scores = fa.weights_

tabela_scores = pd.DataFrame(scores)
tabela_scores.columns = [f"Fator {i+1}" for i, v in enumerate(tabela_scores.columns)]
tabela_scores.index = paises_pca.columns
tabela_scores

print(tabela_scores)

#%% Criando um ranking

paises['Ranking'] = 0

for index, item in enumerate(list(tabela_eigen.index)):
    variancia = tabela_eigen.loc[item]['Variância']

    paises['Ranking'] = paises['Ranking'] + paises[tabela_eigen.index[index]]*variancia
    
#%% Gráfico da variância acumulada dos componentes principais

plt.figure(figsize=(12,8))

plt.title(f"{tabela_eigen.shape[0]} componentes principais que explicam {round(tabela_eigen['Variância'].sum()*100,2)}% da variância", fontsize=14)
ax = sns.barplot(x=tabela_eigen.index, y=tabela_eigen['Variância'], data=tabela_eigen, color='green')

ax.bar_label(ax.containers[0])
plt.xlabel("Componentes principais", fontsize=14)
plt.ylabel("Porcentagem de variância explicada (%)", fontsize=14)
plt.show()

#%% FIM!