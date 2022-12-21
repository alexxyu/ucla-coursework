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

"""
Code Challenge: Solve the Middle Edge in Linear Space Problem (for protein strings).

Input: Two amino acid strings.
Output: A middle edge in the alignment graph in the form "(i, j) (k, l)", where (i, j) connects to
    (k, l). To compute scores, use the BLOSUM62 scoring matrix and a (linear) indel penalty equal to 5.
"""
def middle_edge(str1, str2):
    INDEL_PENALTY = 5

    n, m = len(str1), len(str2)
    mid = m//2

    dp_forward = [0 for _ in range(n+1)]
    for i in range(1, n+1):
        dp_forward[i] = dp_forward[i-1] - INDEL_PENALTY
    for j in range(1, mid+1):
        dp_new = [0 for _ in range(n+1)]
        dp_new[0] = -(j*INDEL_PENALTY)
        for i in range(1, n+1):
            match_score = scoring_matrix[str1[i-1]][str2[j-1]]
            dp_new[i] = max(dp_forward[i-1] + match_score, dp_new[i-1] - INDEL_PENALTY, dp_forward[i] - INDEL_PENALTY)

        dp_forward = dp_new
        
    dp_backward = [0 for _ in range(n+1)]
    backtrack = [(n, m) for _ in range(n+1)]
    for i in range(n-1, -1, -1):
        dp_backward[i] = dp_backward[i+1] - INDEL_PENALTY
    for j in range(m-1, mid-1, -1):
        dp_new = [0 for _ in range(n+1)]
        dp_new[-1] = -(j*INDEL_PENALTY)
        for i in range(n-1, -1, -1):
            match_score = scoring_matrix[str1[i]][str2[j]]
            dp_new[i] = max(dp_backward[i+1] + match_score, dp_new[i+1] - INDEL_PENALTY, dp_backward[i] - INDEL_PENALTY)

            if dp_new[i] == dp_backward[i+1] + match_score:
                backtrack[i] = (i+1, j+1)
            elif dp_new[i+1] - INDEL_PENALTY:
                backtrack[i] = (i+1, j)
            else:
                backtrack[i] = (i, j+1)

        dp_backward = dp_new

    middle_node, next_node, max_score = None, None, -10000000
    for i in range(n+1):
        length = dp_forward[i] + dp_backward[i]
        if length > max_score:
            max_score = length
            middle_node = (i, mid)
            next_node = backtrack[i]

    return middle_node, next_node, max_score

"""
Code Challenge: Implement LinearSpaceAlignment to solve the Global Alignment Problem for a large 
    dataset.

Input: Two long (10000 amino acid) protein strings written in the single-letter amino acid alphabet.
Output: The maximum alignment score of these strings, followed by an alignment achieving this 
    maximum score. Use the BLOSUM62 scoring matrix and indel penalty σ = 5.

Note: This is broken.
"""
def linear_space_alignment(v, w, top, bottom, left, right, s1_path, s2_path):
    if left == right:
        for i in range(top, bottom):
            s1_path.append(v[i])
            s2_path.append('-')
        return
    if top == bottom:
        for i in range(left, right):
            s1_path.append('-')
            s2_path.append(w[i])
        return

    middle = (left + right) // 2
    mid_node, next_node, score = middle_edge(v[top:bottom], w[left:right])

    mid_node = tuple(map(sum, zip(mid_node, [top, left])))
    next_node = tuple(map(sum, zip(next_node, [top, left])))

    mid_node_y, mid_node_x = mid_node
    next_node_y, next_node_x = next_node

    linear_space_alignment(v, w, top, mid_node_y, left, middle, s1_path, s2_path)

    if mid_node_x == next_node_x-1 and mid_node_y == next_node_y-1:
        # Diagonal edge
        s1_path.append(v[mid_node_y])
        s2_path.append(w[mid_node_x])
        middle += 1
        mid_node_y += 1
    elif mid_node_x == next_node_x and mid_node_y == next_node_y-1:
        # Down edge
        s1_path.append(v[mid_node_y])
        s2_path.append('-')
        mid_node_y += 1
    elif mid_node_x == next_node_x-1 and mid_node_y == next_node_y:
        # Right edge
        s1_path.append('-')
        s2_path.append(w[mid_node_x])
        middle += 1

    linear_space_alignment(v, w, mid_node_y, bottom, middle, right, s1_path, s2_path)
    return score

def space_efficient_alignment(file):
    with open(file, 'r') as f:
        str1, str2 = f.read().splitlines()

    s1_path, s2_path = [], []
    score = linear_space_alignment(str1, str2, 0, len(str1), 0, len(str2), s1_path, s2_path)

    return ''.join(s1_path), ''.join(s2_path), score

"""
In the Multiple Longest Common Subsequence Problem, the score of a column of the alignment matrix 
    is equal to 1 if all of the column's symbols are identical, and 0 if even one symbol disagrees.

Code Challenge: Solve the Multiple Longest Common Subsequence Problem.

Input: Three DNA strings of length at most 10.
Output: The length of a longest common subsequence of these three strings, followed by a multiple 
    alignment of the three strings corresponding to such an alignment.
"""
def longest_common_subseq(file):
    insert_indel = lambda word, i: word[:i] + '-' + word[i:]

    def backtracker_3d(backtrack, s1, s2, s3, x, y, z):
        s1_aligned, s2_aligned, s3_aligned = "", "", ""
        i, j, k = x, y, z
        while i != 0 and j != 0 and k != 0:
            if backtrack[i][j][k] == 0:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                s3_aligned = s3[k-1] + s3_aligned
                i -= 1
                j -= 1
                k -= 1
            elif backtrack[i][j][k] == 1:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                s3_aligned = '-' + s3_aligned
                i -= 1
                j -= 1
            elif backtrack[i][j][k] == 2:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = '-' + s2_aligned
                s3_aligned = s3[k-1] + s3_aligned
                i -= 1
                k -= 1
            elif backtrack[i][j][k] == 3:
                s1_aligned = '-' + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                s3_aligned = s3[k-1] + s3_aligned
                j -= 1
                k -= 1
            elif backtrack[i][j][k] == 4:
                s1_aligned = s1[i-1] + s1_aligned
                s2_aligned = '-' + s2_aligned
                s3_aligned = '-' + s3_aligned
                i -= 1
            elif backtrack[i][j][k] == 5:
                s1_aligned = '-' + s1_aligned
                s2_aligned = s2[j-1] + s2_aligned
                s3_aligned = '-' + s3_aligned
                j -= 1
            elif backtrack[i][j][k] == 6:
                s1_aligned = '-' + s1_aligned
                s2_aligned = '-' + s2_aligned
                s3_aligned = s3[k-1] + s3_aligned
                k -= 1

        while i != 0:
            s1_aligned = s1[i-1] + s1_aligned
            i -= 1
        while j != 0:
            s2_aligned = s2[j-1] + s2_aligned
            j -= 1
        while k != 0:
            s3_aligned = s3[k-1] + s3_aligned
            k -= 1

        while len(s1_aligned) != max(len(s1_aligned),len(s2_aligned),len(s3_aligned)):
            s1_aligned = insert_indel(s1_aligned, 0)
        while len(s2_aligned) != max(len(s1_aligned),len(s2_aligned),len(s3_aligned)):
            s2_aligned = insert_indel(s2_aligned, 0)
        while len(s3_aligned) != max(len(s1_aligned),len(s2_aligned),len(s3_aligned)):
            s3_aligned = insert_indel(s3_aligned, 0)
        
        return s1_aligned, s2_aligned, s3_aligned

    with open(file, 'r') as f:
        s1, s2, s3 = f.read().splitlines()
        x, y, z = len(s1), len(s2), len(s3)

    dp = [[[0 for _ in range(z+1)] for _ in range(y+1)] for _ in range(x+1)]
    backtrack = [[[0 for _ in range(z+1)] for _ in range(y+1)] for _ in range(x+1)]

    for i in range(1, x+1):
        for j in range(1, y+1):
            for k in range(1, z+1):
                match_score = 1 if s1[i-1] == s2[j-1] and s2[j-1] == s3[k-1] else 0
                scores = [dp[i-1][j-1][k-1]+match_score, dp[i-1][j-1][k], dp[i-1][j][k-1], dp[i][j-1][k-1], dp[i-1][j][k], dp[i][j-1][k], dp[i][j][k-1]]
                backtrack[i][j][k], dp[i][j][k] = max(enumerate(scores), key=lambda p: p[1])

    return *backtracker_3d(backtrack, s1, s2, s3, x, y, z), dp[x][y][z]

s1, s2, s3, score = longest_common_subseq('alignment.txt')
print(score)
print(s1)
print(s2)
print(s3)

# s1, s2, score = space_efficient_alignment('alignment.txt')
# print(score)
# print(s1)
# print(s2)