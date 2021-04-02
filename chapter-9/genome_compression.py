import time

"""
Compress text using Burrows-Wheeler Transform (BWT)
"""
def compress(file):
    with open(file, 'r') as f:
        text = f.read().splitlines()[0]
        matrix = [text]
        curr = text
        while True:
            curr = curr[-1] + curr[:-1]
            if curr == text:
                break
            matrix.append(curr)

        matrix.sort()
        BWT = ""
        for s in matrix:
            BWT += s[-1]
        print(BWT)

"""
Decompress text using Burrows-Wheeler Transform (BWT)
"""
def decompress(file):
    with open(file, 'r') as f:
        transform = f.read().splitlines()[0]
        
        text = ""
        last_column = [c for c in transform]
        first_column = sorted(last_column)

        char_to_find = '$'
        occurence_to_find = 0
        while True:
            char_index = [i for i, c in enumerate(last_column) if c == char_to_find][occurence_to_find]
            char_to_find = first_column[char_index]
            
            occurence_to_find = 0
            i = char_index - 1
            while i >= 0 and first_column[i] == char_to_find:
                occurence_to_find += 1
                i -= 1

            text += char_to_find
            if char_to_find == '$':
                break
        
        char_to_find += '$'
        print(text)

"""
Match text in BWT form to list of patterns and get counts
"""
def bwt_match(file):
    with open(file, 'r')  as f:
        bwt, patterns = f.read().splitlines()
        patterns = patterns.split()

        last_column = [c for c in bwt]
        first_column = sorted(last_column)

        occ_count = dict()
        last_to_first = dict()

        # Generate last-to-first mapping
        for i, c in enumerate(last_column):
            if c not in occ_count.keys():
                occ_count[c] = 0

            target = occ_count[c]
            first_col_pos = 0
            while True:
                while first_column[first_col_pos] != c:
                    first_col_pos += 1
                if target == 0:
                    break
                target -= 1
                first_col_pos += 1

            last_to_first[i] = first_col_pos
            occ_count[c] += 1

        occurences = []
        for pattern in patterns:
            top = 0
            bottom = len(last_column) - 1
            while top <= bottom:
                if len(pattern) > 0:
                    symbol = pattern[-1]
                    pattern = pattern[:-1]

                    if symbol in last_column[top:bottom+1]:
                        symbol_pos = [i+top for i, c in enumerate(last_column[top:bottom+1]) if c == symbol]
                        topIndex = symbol_pos[0]
                        bottomIndex = symbol_pos[-1]
                        top = last_to_first[topIndex]
                        bottom = last_to_first[bottomIndex]
                    else:
                        occurences.append("0")
                        break
                else:
                    occurences.append(str(bottom - top + 1))
                    break

        print(' '.join(occurences))

"""
better_bwt_match is ~ 4x faster than bwt_match by substituting the last-to-first mapping with count arrays
"""
def better_bwt_match(file):
    with open(file, 'r')  as f:
        bwt, patterns = f.read().splitlines()
        patterns = patterns.split()

        last_column = [c for c in bwt]
        first_column = sorted(last_column)

        # Calculate counts for each character up to each index in last_column
        counts = {c: [0] for c in list(set(last_column))}
        for i in range(len(last_column)):
            c = last_column[i]
            for k in counts.keys():
                if k == c:
                    counts[k].append(counts[k][-1]+1)
                else:
                    counts[k].append(counts[k][-1])

        # Find first occurence of each character in first_column
        first_occurrence = dict()
        for c in counts.keys():
            first_occurrence[c] = [i for i, k in enumerate(first_column) if k == c][0]

        occurences = []
        for pattern in patterns:
            top = 0
            bottom = len(last_column) - 1
            while top <= bottom:
                if len(pattern) > 0:
                    symbol = pattern[-1]
                    pattern = pattern[:-1]

                    if symbol in last_column[top:bottom+1]:
                        top = first_occurrence[symbol] + counts[symbol][top]
                        bottom = first_occurrence[symbol] + counts[symbol][bottom+1] - 1
                    else:
                        occurences.append("0")
                        break
                else:
                    occurences.append(str(bottom - top + 1))
                    break

        print(' '.join(occurences))

better_bwt_match("genome_compression.txt")