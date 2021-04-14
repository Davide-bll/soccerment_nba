from numpy import dot
from numpy.linalg import norm
from itertools import combinations


def compute_max_distance(df, list_players):
    comb_players = list(combinations(list_players, 2))
    list_dist = []
    for comb in comb_players:
        list_dist.append(compute_euclidean_distance(df, comb[0], comb[1]))

    return max(list_dist)


def compute_euclidean_distance(df, player1, player2):
    return norm(df[player1] - df[player2])


def compute_relative_similarity(df, player1, player2, max_distance):
    return 1 - (norm(df[player1] - df[player2]) / max_distance)


def compute_complementarity(df, player1, player2, max_distance):
    return 1 - compute_relative_similarity(df, player1, player2, max_distance)
