import pandas as pd
import matplotlib.pyplot as plt

# parameters
save_figures = False
# load data
players = pd.read_csv("data/players_post_input.csv")
scl_ftrs = pd.read_csv("data/scaled_features_post_input.csv")

# correlation matrix
corr_all_vars = scl_ftrs.corr()
plt.matshow(corr_all_vars)
plt.xticks(range(scl_ftrs.select_dtypes(['number']).shape[1]), scl_ftrs.select_dtypes(['number']).columns, fontsize=7, rotation=45)
plt.yticks(range(scl_ftrs.select_dtypes(['number']).shape[1]), scl_ftrs.select_dtypes(['number']).columns, fontsize=10)
cb = plt.colorbar()
cb.ax.tick_params(labelsize=14)
if save_figures:
    plt.savefig('corr.png')

high_corr_vars = ['FGM', 'FGA', 'FTM', 'FTA', '3PA', '3PM', '2PA', '2PM', 'DREB', 'OREB']
df_high_corr_vars = scl_ftrs[high_corr_vars].corr()
plt.matshow(df_high_corr_vars)
plt.xticks(range(scl_ftrs[high_corr_vars].select_dtypes(['number']).shape[1]), scl_ftrs[high_corr_vars].select_dtypes(['number']).columns, fontsize=7, rotation=45)
plt.yticks(range(scl_ftrs[high_corr_vars].select_dtypes(['number']).shape[1]), scl_ftrs[high_corr_vars].select_dtypes(['number']).columns, fontsize=10)
cb = plt.colorbar()
cb.ax.tick_params(labelsize=14)
if save_figures:
    plt.savefig('high_corr_vars.png')

# from observation of the corr plot, it make sense to ecxlude:
# FGM, FGA -> work with FG_pc
# 3PM, 3PA -> work with 3P_pc
# 2PM, 2PA -> work with 2P_PC
# REB -> work with OREB adnd DREB instead (even if the value of corr is high: 0,7)
vars_to_exclude = ['FGM', 'FGA', 'FTM', 'FTA', '3PA', '3PM', '2PA', '2PM', 'REB']

players = players[[col for col in sorted(list(players.columns)) if not (col in vars_to_exclude)]]
scl_ftrs = scl_ftrs[[col for col in sorted(list(scl_ftrs.columns)) if not (col in vars_to_exclude)]]

# save to csv (reset index so that it fits the shinyapp
players.to_csv("data/players_post_corr.csv", index=False)
scl_ftrs.to_csv("data/scaled_features_post_corr.csv", index=False)
