# Load scoring matrix
scoring_matrix = dict()
with open('PAM250.txt', 'r') as f:
    lines = f.read().splitlines()
    alphabet = lines[0].strip().split()
    
    scoring_matrix = dict()
    for line in lines[1:]:
        scores = line.split()
        k, scores = scores[0], scores[1:]
        scoring_matrix[k] = dict()

        for a, s in zip(alphabet, scores):
            scoring_matrix[k][a] = int(s)

"""
Code Challenge: Solve the Global Alignment Problem.

Input: Two protein strings written in the single-letter amino acid alphabet.
Output: The maximum alignment score of these strings followed by an alignment achieving this 
    maximum score. Use the BLOSUM62 scoring matrix for matches and mismatches as well as the 
    indel penalty σ = 5.
"""
def global_alignment(file):
    INDEL_PENALTY = 5

    def backtracker(backtrack, s1, s2, n, m):
        s1_aligned, s2_aligned = "", ""
        i, j = n, m
        while i != 0 or j != 0:
            if backtrack[i][j] == 'd':
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = '-' + s2_aligned
                i -= 1
            elif backtrack[i][j] == 'r':
                s1_aligned = '-' + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                j -= 1
            else:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                i -= 1
                j -= 1
        return s1_aligned, s2_aligned

    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()
        n, m = len(str1), len(str2)

    dp = [[0 for _ in range(m+1)] for _ in range(n+1)]
    backtrack = [['' for _ in range(m+1)] for _ in range(n+1)]

    for i in range(1, n+1):
        dp[i][0] = dp[i-1][0] - INDEL_PENALTY
        backtrack[i][0] = 'd'
    for j in range(1, m+1):
        dp[0][j] = dp[0][j-1] - INDEL_PENALTY
        backtrack[0][j] = 'r'

    for i in range(1, n+1):
        for j in range(1, m+1):
            match_score = scoring_matrix[str1[i-1]][str2[j-1]]
            dp[i][j] = max(dp[i-1][j]-INDEL_PENALTY, dp[i][j-1]-INDEL_PENALTY, dp[i-1][j-1]+match_score)

            if dp[i][j] == dp[i-1][j]-INDEL_PENALTY:
                backtrack[i][j] = 'd'
            elif dp[i][j] == dp[i][j-1]-INDEL_PENALTY:
                backtrack[i][j] = 'r'
            elif dp[i][j] == dp[i-1][j-1]+match_score:
                backtrack[i][j] = 'v'

    return *backtracker(backtrack, str1, str2, n, m), dp[n][m]

"""
Code Challenge: Solve the Local Alignment Problem.

Input: Two protein strings written in the single-letter amino acid alphabet.
Output: The maximum score of a local alignment of the strings, followed by a local alignment of 
    these strings achieving the maximum score. Use the PAM250 scoring matrix for matches and 
    mismatches as well as the indel penalty σ = 5.
"""
def local_alignment(file):
    INDEL_PENALTY = 5

    def backtracker(backtrack, s1, s2, n, m):
        s1_aligned, s2_aligned = "", ""
        i, j = n, m
        while i != 0 or j != 0:
            if backtrack[i][j] == 's':
                break
            elif backtrack[i][j] == 'd':
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = '-' + s2_aligned
                i -= 1
            elif backtrack[i][j] == 'r':
                s1_aligned = '-' + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                j -= 1
            else:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                i -= 1
                j -= 1
        return s1_aligned, s2_aligned

    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()
        n, m = len(str1), len(str2)

    dp = [[0 for _ in range(m+1)] for _ in range(n+1)]
    backtrack = [['' for _ in range(m+1)] for _ in range(n+1)]

    for i in range(1, n+1):
        dp[i][0] = dp[i-1][0] - INDEL_PENALTY
        backtrack[i][0] = 'd'
    for j in range(1, m+1):
        dp[0][j] = dp[0][j-1] - INDEL_PENALTY
        backtrack[0][j] = 'r'

    for i in range(1, n+1):
        for j in range(1, m+1):
            match_score = scoring_matrix[str1[i-1]][str2[j-1]]
            dp[i][j] = max(0, dp[i-1][j]-INDEL_PENALTY, dp[i][j-1]-INDEL_PENALTY, dp[i-1][j-1]+match_score)

            if dp[i][j] == 0:
                backtrack[i][j] = 's'
            if dp[i][j] == dp[i-1][j]-INDEL_PENALTY:
                backtrack[i][j] = 'd'
            elif dp[i][j] == dp[i][j-1]-INDEL_PENALTY:
                backtrack[i][j] = 'r'
            elif dp[i][j] == dp[i-1][j-1]+match_score:
                backtrack[i][j] = 'v'

    x, y = n, m
    max_val = 0
    for i in range(n+1):
        for j in range(m+1):
            if dp[i][j] > max_val:
                max_val = dp[i][j]
                x, y = i, j

    return *backtracker(backtrack, str1, str2, x, y), dp[x][y]

def edit_distance(file):
    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()
        n, m = len(str1), len(str2)

    dp = [[0 for _ in range(m+1)] for _ in range(n+1)]
    for i in range(1, n+1):
        dp[i][0] = i
    for j in range(1, m+1):
        dp[0][j] = j

    for i in range(1, n+1):
        for j in range(1, m+1):
            match = 0 if str1[i-1] == str2[j-1] else 1
            dp[i][j] = min(dp[i-1][j-1]+match, dp[i-1][j]+1, dp[i][j-1]+1)

    return dp[n][m]

s1, s2, score = local_alignment('alignment.txt')
print(score)
print(s1)
print(s2)