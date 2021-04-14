# prettify string
def prettify_string(x, sub=""):
    return x.replace(" ", sub).lower()

def create_list_col(n, base="pc"):
    return list(map(lambda x: base + str(x+1), range(0, n)))


def find_type_cols(df, include=['int16', 'int32', 'int64', 'float16', 'float32', 'float64']):
    included_cols = df.select_dtypes(include=include)

    return list(included_cols)


def transpose_features(df):
    return df.set_index(df['ID'])[find_type_cols(df)].T


def extract_players_with_role(df, role="any"):
    if role == "any":
        bool_index = [True for elem in range(0, len(df.index))]
    else:
        bool_index = df['Pos'] == role
    return list(df[bool_index]['ID'])
