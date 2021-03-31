import time

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

bwt_match("genome_compression.txt")
