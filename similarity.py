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

# transpose features
t_scl_ftrs = transpose_features(scl_ftrs)

# computation last about 5 seconds
list_players = extract_players_with_role(players, "any")
overall_max_distance = compute_max_distance(t_scl_ftrs, list_players)

comb_players = list(combinations(list_players, 2))
comb_df = pd.DataFrame(comb_players, columns=['Player1', 'Player2'])
start_time = datetime.now()
comb_df['similarity'] = list(
    map(lambda x, y: compute_relative_similarity(t_scl_ftrs, x, y, overall_max_distance), comb_df['Player1'],
        comb_df['Player2']))
end_time = datetime.now()

print("Time spent to compute overall simlarity with 261 players: ")
print(end_time - start_time)
comb_df['complementarity'] = 1 - comb_df['similarity']

# save result
comb_df.to_csv("output_data/ovrl_distances.csv", index=False)
