from datetime import datetime
import pandas as pd
from itertools import combinations
from helpers import extract_players_with_role, transpose_features
from helpers_similarity import compute_max_distance, compute_relative_similarity

# load data
players = pd.read_csv("data/players_post_corr.csv")
scl_ftrs = pd.read_csv("data/scaled_features_post_corr.csv")
list_roles = players['Pos'].unique()
# keep only 1 roles (for now)
list_roles = [role for role in list_roles if not ("-" in role)]
# overall complementarity
df_overall = pd.read_csv("output_data/ovrl_distances.csv")

# transpose features
t_scl_ftrs = transpose_features(scl_ftrs)

# computation last about 5 seconds
list_players = [extract_players_with_role(players, elem) for elem in list_roles]
# use a dictionary to recognize list of which roles
dict_players_roles = dict(zip(list_roles, list_players))
# compute max distances per role
dict_max_distance = dict(zip(list_roles, [compute_max_distance(t_scl_ftrs, elem) for elem in dict_players_roles.values()]))

# compute combination for every role
comb_roles_players_values = [list(combinations(elem, 2)) for elem in dict_players_roles.values()]
# create dataframe of combinations per every role
dict_comb_roles_players = dict(zip(list_roles, [pd.DataFrame(elem, columns=['Player1', 'Player2']) for elem in comb_roles_players_values]))

# for every role, com
for role in dict_comb_roles_players.keys():
    dict_comb_roles_players[role]['similarity'] = list(
        map(lambda x, y: compute_relative_similarity(
            t_scl_ftrs,
            x,
            y,
            dict_max_distance[role]),
            dict_comb_roles_players[role]['Player1'],
            dict_comb_roles_players[role]['Player2']))
    dict_comb_roles_players[role]['complementarity'] = 1 - dict_comb_roles_players[role]['similarity']

# add role to every dataframe and concatenate
for role in dict_comb_roles_players.keys():
    dict_comb_roles_players[role].insert(0, 'Pos', [role]*len(dict_comb_roles_players[role]))

df_by_roles = pd.concat(dict_comb_roles_players.values())

# add_overall complementarity ###
# change column name of overall_df
df_overall.rename(columns={'similarity': 'similarity_ov', 'complementarity': 'complementarity_ov'}, inplace=True)
# merge by role and overall
df_by_roles_overall = df_by_roles.merge(df_overall, how='left', on=['Player1', 'Player2'])

# add column of delta between two method
df_by_roles_overall['delta_sim'] = df_by_roles_overall['similarity_ov'] - df_by_roles_overall['similarity']
df_by_roles_overall['delta_com'] = df_by_roles_overall['complementarity'] - df_by_roles_overall['complementarity_ov']

# save output to csv
df_by_roles_overall.to_csv("output_data/by_roles_overall_distances.csv", index=False)
