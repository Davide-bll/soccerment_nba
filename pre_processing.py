import pandas as pd
from sklearn.decomposition import PCA
import numpy as np
from helpers import find_type_cols, create_list_col

# load data
players = pd.read_csv("data/players_post_corr.csv")
scl_ftrs = pd.read_csv("data/scaled_features_post_corr.csv")

# set index
scl_ftrs = scl_ftrs.set_index(scl_ftrs['ID'])[find_type_cols(scl_ftrs)]
# pca
pca = PCA(n_components=len(scl_ftrs.columns))
df_pc = pd.DataFrame(data=pca.fit_transform(scl_ftrs), columns=create_list_col(len(scl_ftrs.columns)), index=scl_ftrs.index)
loadings = pca.components_.T * np.sqrt(pca.explained_variance_)
df_loadings = pd.DataFrame(loadings, columns=df_pc.columns, index=list(scl_ftrs.columns))
# attach role to df of pca
df_pc = df_pc.reset_index().merge(players[['Pos', 'Player', 'ID']], how='left', on=["ID"])

# save output to visualize in shinyapp
df_pc.to_csv("data/df_principal_componenets_scld.csv", index=False)

# note:
# interpretazione pca1: tutte le variabili spiegano variabilità, i loadeds sono simili
# guarda differenza su pca2, le features simili che hanno scores opposto
# plot prime due pc rispetto ai diversi ruoli
# gestisci ruoli multipli! (sono solo 6 osservazioni)
#
#   step1: distanza euclidea/cosine distance sui dati standardizzati? come similarity
#   tipo: similarity = dist/dist_max
#   step2: dist/dist_max per ruolo
#   distanza da decidere: se standardizzo i dati sto già tenendo conto della magnitude delle features
#   quindi sarebbe meglio utilizzare la distanza coseno
#   visuaòization:
#   - spider chart
#   - distanza sulle pca
# sarebbe bello capire quali sono le features piu importanti per ruolo, tipo fare una PCA per ogni ruolo