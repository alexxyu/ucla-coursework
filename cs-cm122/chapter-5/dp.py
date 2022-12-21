import sys
import math

'''
Code Challenge: Solve the Change Problem. The DPChange pseudocode is reproduced below for your 
convenience.

Input: An integer money and an array Coins = (coin1, ..., coind).
Output: The minimum number of coins with denominations Coins that changes money.
'''
def dp_change(file):
    with open(file, 'r') as f:
        money, coins = f.read().splitlines()
        money = int(money)
        coins = [int(c) for c in coins.split(',')]

    dp = [math.inf for _ in range(money+1)]
    dp[0] = 0
    for m in range(1, money+1):
        for c in coins:
            if m-c >= 0:
                dp[m] = min(dp[m], dp[m-c]+1)

    return dp[m]

'''
Code Challenge: Find the length of a longest path in the Manhattan Tourist Problem.

Input: Integers n and m, followed by an n × (m + 1) matrix Down and an (n + 1) × m matrix Right. 
    The two matrices are separated by the "-" symbol.
Output: The length of a longest path from source (0, 0) to sink (n, m) in the rectangular grid 
    whose edges are defined by the matrices Down and Right.
'''
def manhattan_tourist_length(file):
    with open(file, 'r') as f:
        lines = f.read().splitlines()
        n, m = [int(k) for k in lines[0].split(' ')]
        down, right = [], []

        for line in lines[1:n+1]:
            down.append([int(k) for k in line.split(' ')])
        for line in lines[n+2:]:
            right.append([int(k) for k in line.split(' ')])

    dp = [[0 for _ in range(m+1)] for _ in range(n+1)]

    for i in range(1, n+1):
        dp[i][0] = dp[i-1][0] + down[i-1][0]
    for j in range(1, m+1):
        dp[0][j] = dp[0][j-1] + right[0][j-1]

    for i in range(1, n+1):
        for j in range(1, m+1):
            dp[i][j] = max(dp[i-1][j] + down[i-1][j], dp[i][j-1] + right[i][j-1])

    return dp[n][m]

def longest_common_subseq(file):
    def backtracker(backtrack, v, n, m):
        reconstructed = ""
        i, j = n, m
        while i != 0 and j != 0:
            if backtrack[i][j] == 'd':
                i -= 1
            elif backtrack[i][j] == 'r':
                j -= 1
            else:
                reconstructed = v[i-1] + reconstructed
                i -= 1
                j -= 1
        return reconstructed

    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()
    
    n, m = len(str1), len(str2)
    backtrack = [['' for _ in range(m+1)] for _ in range(n+1)]

    dp = [[0 for _ in range(m+1)] for _ in range(n+1)]
    for i in range(1, n+1):
        for j in range(1, m+1):
            match = 0
            if str1[i-1] == str2[j-1]:
                match = 1

            dp[i][j] = max(dp[i-1][j], dp[i][j-1], dp[i-1][j-1]+match)

            if dp[i][j] == dp[i-1][j]:
                backtrack[i][j] = 'd'
            elif dp[i][j] == dp[i][j-1]:
                backtrack[i][j] = 'r'
            elif dp[i][j] == dp[i-1][j-1]+match:
                backtrack[i][j] = 'v'

    return backtracker(backtrack, str1, n, m)

'''
Code Challenge: Solve the Longest Path in a DAG Problem.

Input: An integer representing the starting node to consider in a graph, followed by an integer 
    representing the ending node to consider, followed by a list of edges in the graph. The edge 
    notation "0->1:7" indicates that an edge connects node 0 to node 1 with weight 7.  You may 
    assume a given topological order corresponding to nodes in increasing order.
Output: The length of a longest path in the graph, followed by a longest path. (If multiple 
    longest paths exist, you may return any one.)
'''
def longest_path_in_dag(file):
    with open(file, 'r') as f:
        lines = f.read().splitlines()
        start = int(lines[0])
        end = int(lines[1])

        reversed_edges = {k:[] for k in range(end+1)}
        for line in lines[2:]:
            edge, weight = line.split(':')
            weight = int(weight)

            a, b = edge.split('->')
            a = int(a)
            b = int(b)

            reversed_edges[b].append((a, weight))

    dist = [0 for _ in range(end+1)]
    backtrack = [None for _ in range(end+1)]
    for n in range(1, end+1):
        for (a, weight) in reversed_edges[n]:
            new_dist = dist[a] + weight
            if new_dist >= dist[n]:
                dist[n] = new_dist
                backtrack[n] = a

    path = []
    curr_node = end
    while curr_node != start:
        path.append(curr_node)
        curr_node = backtrack[curr_node]

    path.append(start)
    path.reverse()
    return dist[end], path

dist, path = longest_path_in_dag('dp.txt')
print(dist)
print('->'.join([str(s) for s in path]))