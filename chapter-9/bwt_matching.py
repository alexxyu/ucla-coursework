def construct_suffix_array(text):
    arr = []
    for i in range(len(text)):
        arr.append((text[i:], i))
    return [x[1] for x in sorted(arr, key=lambda x: x[0])]

def find_pos_in_text(text, pattern, suffix_array):
    # Search for the pattern in the suffix array
    min_idx = 0
    max_idx = len(suffix_array)-1
    while min_idx < max_idx:
        mid_idx = (min_idx + max_idx) // 2
        if pattern > text[suffix_array[mid_idx]:]:
            min_idx = mid_idx + 1
        else:
            max_idx = mid_idx

    if pattern != text[suffix_array[min_idx] : suffix_array[min_idx]+len(pattern)]:
        return []
    first = min_idx

    # Iterate to find first and last occurence of the pattern in the suffix array
    while first >= 0 and pattern == text[suffix_array[first] : suffix_array[first]+len(pattern)]:
        first -= 1
    first += 1

    last = first
    while last < len(suffix_array) and pattern == text[suffix_array[last]:suffix_array[last]+len(pattern)]:
        last += 1
    return [suffix_array[idx] for idx in range(first, last)]

"""
Code Challenge: Solve the Multiple Pattern Matching Problem.

Input: A string Text followed by a collection of strings Patterns.
Output: All starting positions in Text where a string from Patterns appears as a substring.
"""
def suffix_array_match(file):
    with open(file, 'r') as f:
        lines = f.read().splitlines()
        text = lines[0]
        patterns = lines[1:]

        suffix_array = construct_suffix_array(text)
        matching_pos = []
        for p in patterns:
            matching_pos.extend(find_pos_in_text(text, p, suffix_array))
        matching_pos.sort()
        print(' '.join([str(p) for p in matching_pos]))

"""
Burrows-Wheeler Transform Construction Problem: Construct the Burrows-Wheeler transform of a string.

Input: A string Text.
Output: BWT(Text).
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
Inverse Burrows-Wheeler Transform Problem: Reconstruct a string from its Burrows-Wheeler transform.

Input: A string Transform (with a single "$" symbol).
Output: The string Text such that BWT(Text) = Transform.
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
Implement BWMatching.

Input: A string BWT(Text), followed by a collection of Patterns.
Output: A list of integers, where the i-th integer corresponds to the number of substring matches of the i-th member of Patterns in Text.
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

        #print(' '.join(occurences))

suffix_array_match("bwt_matching.txt")