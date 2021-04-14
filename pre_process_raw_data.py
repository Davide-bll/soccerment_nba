import pandas as pd
from sklearn.preprocessing import MinMaxScaler
from helpers import find_type_cols

# load raw data
players = pd.read_csv("raw_data/players.csv")
raw_roles = pd.read_csv("raw_data/nbastats.csv")

# select role played for max number of season
roles = raw_roles[['ID', 'Player', 'Pos']]
roles = roles.groupby(by=['ID', 'Pos', 'Player'])[['Player']].count().rename(columns={'Player': 'n_season'}).reset_index()
# some players have multiple roles with max value of n_season.. I'll deal with it later
roles = roles.groupby(['ID'])[['Player', 'Pos']].max().reset_index()
players = players.merge(roles, how='left', on='Player')

# deal with NA values
missing_players = pd.isna(players['ID']) | pd.isna(players['Pos'])
merge_cols = [col for col in list(players.columns) if (col in list(raw_roles.columns)) and (col not in ['Pos', 'ID', 'Player'])]
right_df = raw_roles[['Pos', 'ID'] + merge_cols].drop_duplicates()
new_vals = players[missing_players][['Player'] + merge_cols].merge(right_df, how='left', on=merge_cols)
new_vals.set_index(players[missing_players].index, inplace=True)
players.fillna(value=new_vals[['ID', 'Pos']], inplace=True)

# create 2p attempt stats
players['2PA']   = players['FGA'] - players['3PA']
players['2PM']   = players['FGM'] - players['3PM']
players['2P_pc'] = players['2PM'] / players['2PA']


scaler = MinMaxScaler()
ftrs = players[find_type_cols(players)]
# scale variables to best compare variability of the data: some of them have a big magnitude with respect to the other
scl_ftrs = pd.DataFrame(scaler.fit_transform(ftrs), columns=list(ftrs.columns), index=players['ID'])


# save csv to use in shiny app
players.to_csv("data/players_post_input.csv", index=False)
scl_ftrs.reset_index().to_csv("data/scaled_features_post_input.csv", index=False)
