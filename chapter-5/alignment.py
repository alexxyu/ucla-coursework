# Load scoring matrix
scoring_matrix = dict()
with open('BLOSUM62.txt', 'r') as f:
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
General backtracker that returns the aligned sequences
"""
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

"""
Code Challenge: Solve the Global Alignment Problem.

Input: Two protein strings written in the single-letter amino acid alphabet.
Output: The maximum alignment score of these strings followed by an alignment achieving this 
    maximum score. Use the BLOSUM62 scoring matrix for matches and mismatches as well as the 
    indel penalty σ = 5.
"""
def global_alignment(file):
    INDEL_PENALTY = 5

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

"""
Edit Distance Problem: Find the edit distance between two strings.

Input: Two strings.
Output: The edit distance between these strings.
"""
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

"""
Code Challenge: Solve the Fitting Alignment Problem.

Input: Two nucleotide strings v and w, where v has length at most 1000 and w has length at most 100
Output: A highest-scoring fitting alignment between v and w. Use the simple scoring method in which 
    matches count +1 and both the mismatch and indel penalties are 1.
"""
def fitting_alignment(file):
    INDEL_PENALTY = 1

    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()
        n, m = len(str1), len(str2)

    dp = [[0 for _ in range(m+1)] for _ in range(n+1)]
    backtrack = [['' for _ in range(m+1)] for _ in range(n+1)]

    for i in range(1, n+1):
        # Allow free rides only from the beginning of str1
        dp[i][0] = 0
        backtrack[i][0] = 's'
    for j in range(1, m+1):
        dp[0][j] = dp[0][j-1] - INDEL_PENALTY
        backtrack[0][j] = 'r'

    for i in range(1, n+1):
        for j in range(1, m+1):
            match_score = 1 if str1[i-1] == str2[j-1] else -1
            dp[i][j] = max(dp[i-1][j]-INDEL_PENALTY, dp[i][j-1]-INDEL_PENALTY, dp[i-1][j-1]+match_score)

            if dp[i][j] == dp[i-1][j]-INDEL_PENALTY:
                backtrack[i][j] = 'd'
            elif dp[i][j] == dp[i][j-1]-INDEL_PENALTY:
                backtrack[i][j] = 'r'
            elif dp[i][j] == dp[i-1][j-1]+match_score:
                backtrack[i][j] = 'v'

    x, y = n, m
    max_val = 0
    # Allow free rides only to the end of str1
    for i in range(n+1):
        if dp[i][m] > max_val:
            max_val = dp[i][m]
            x = i

    return *backtracker(backtrack, str1, str2, x, y), dp[x][y]

"""
Code Challenge: Solve the Overlap Alignment Problem.

Input: Two strings v and w, each of length at most 1000.
Output: The score of an optimal overlap alignment of v and w, followed by an alignment of a suffix 
    v' of v and a prefix w' of w achieving this maximum score. Use an alignment score in which 
    matches count +1 and both the mismatch and indel penalties are 2.
"""
def overlap_alignment(file):
    INDEL_PENALTY = 2

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
        # Allow free rides only from the beginning of str1
        dp[i][0] = 0
        backtrack[i][0] = 's'
    for j in range(1, m+1):
        dp[0][j] = dp[0][j-1] - INDEL_PENALTY
        backtrack[0][j] = 'r'

    for i in range(1, n+1):
        for j in range(1, m+1):
            match_score = 1 if str1[i-1] == str2[j-1] else -2
            dp[i][j] = max(dp[i-1][j]-INDEL_PENALTY, dp[i][j-1]-INDEL_PENALTY, dp[i-1][j-1]+match_score)

            if dp[i][j] == dp[i-1][j]-INDEL_PENALTY:
                backtrack[i][j] = 'd'
            elif dp[i][j] == dp[i][j-1]-INDEL_PENALTY:
                backtrack[i][j] = 'r'
            elif dp[i][j] == dp[i-1][j-1]+match_score:
                backtrack[i][j] = 'v'

    x, y = n, m
    max_val = 0
    # Allow free rides only to the end of str2
    for j in range(m+1):
        if dp[n][j] > max_val:
            max_val = dp[n][j]
            y = j

    return *backtracker(backtrack, str1, str2, x, y), dp[x][y]

"""
Code Challenge: Solve the Alignment with Affine Gap Penalties Problem.

Input: Two amino acid strings v and w (each of length at most 100).
Output: The maximum alignment score between v and w, followed by an alignment of v and w achieving 
    this maximum score. Use the BLOSUM62 scoring matrix, a gap opening penalty of 11, and a gap 
    extension penalty of 1.
"""
def alignment_with_affine_gap(file):
    GAP_EXTENSION_PENALTY = 1
    GAP_OPENING_PENALTY = 11

    def gaff_backtracker(backtrack, s1, s2, n, m, k):
        s1_aligned, s2_aligned = "", ""
        i, j = n, m
        while i != 0 or j != 0:
            if backtrack[k][i][j] == 'd':
                if k == 0:
                    s1_aligned = s1[i-1] + s1_aligned
                    s2_aligned = '-' + s2_aligned
                    i -= 1
                k = 0
            elif backtrack[k][i][j] == 'r':
                if k == 2:
                    s2_aligned = s2[j-1] + s2_aligned
                    s1_aligned = '-' + s1_aligned
                    j -= 1
                k = 2
            elif backtrack[k][i][j] == 'b':
                if k == 0:
                    s1_aligned = s1[i-1] + s1_aligned
                    s2_aligned = '-' + s2_aligned
                    i -= 1
                elif k == 2:
                    s2_aligned = s2[j-1] + s2_aligned
                    s1_aligned = '-' + s1_aligned
                    j -= 1
                k = 1
            else:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                i -= 1
                j -= 1

        return s1_aligned, s2_aligned

    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()
        n, m = len(str1), len(str2)

    dp = [[[0 for _ in range(m+1)] for _ in range(n+1)] for _ in range(3)]
    backtrack = [[['' for _ in range(m+1)] for _ in range(n+1)] for _ in range(3)]

    for i in range(1, n+1):
        for j in range(1, m+1):
            match_score = scoring_matrix[str1[i-1]][str2[j-1]]
            
            dp[0][i][j] = max(dp[0][i-1][j] - GAP_EXTENSION_PENALTY, dp[1][i-1][j] - GAP_OPENING_PENALTY)
            if dp[0][i][j] == dp[0][i-1][j] - GAP_EXTENSION_PENALTY:
                backtrack[0][i][j] = 'd'
            else:
                backtrack[0][i][j] = 'b'

            dp[2][i][j] = max(dp[2][i][j-1] - GAP_EXTENSION_PENALTY, dp[1][i][j-1] - GAP_OPENING_PENALTY)
            if dp[2][i][j] == dp[2][i][j-1] - GAP_EXTENSION_PENALTY:
                backtrack[2][i][j] = 'r'
            else:
                backtrack[2][i][j] = 'b'

            dp[1][i][j] = max(dp[0][i][j], dp[1][i-1][j-1] + match_score, dp[2][i][j])
            if dp[1][i][j] == dp[0][i][j]:
                backtrack[1][i][j] = 'd'
            elif dp[1][i][j] == dp[2][i][j]:
                backtrack[1][i][j] = 'r'
            else:
                backtrack[1][i][j] = 'v'

    k = max(enumerate([dp[0][n][m], dp[1][n][m], dp[2][n][m]]), key=lambda x: x[1])[0]
    return *gaff_backtracker(backtrack, str1, str2, n, m, k), dp[k][n][m] 

s1, s2, score = alignment_with_affine_gap('alignment.txt')
print(score)
print(s1)
print(s2)