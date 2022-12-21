class Node:
    def __init__(self, id, char):
        self.id = id
        self.char = char
        self.children = dict()

def insert(string, trie):
    global counter
    curr = root
    for c in string:
        if c not in curr.children.keys():
            curr.children[c] = Node(counter, c)
            counter += 1
        curr = curr.children[c]

"""
Code Challenge: Solve the Trie Construction Problem.

Input: A collection of strings Patterns.
Output: The adjacency list corresponding to Trie(Patterns), in the following format. If Trie(Patterns) has n nodes, 
first label the root with 0 and then label the remaining nodes with the integers 1 through n - 1 in any order you like. 
Each edge of the adjacency list of Trie(Patterns) will be encoded by a triple: the first two members of the triple must 
be the integers labeling the initial and terminal nodes of the edge, respectively; the third member of the triple must 
be the symbol labeling the edge.
"""
def trie_matching(text, trie):
    k = 0
    all_pos = []
    while len(text) > 0:
        if prefix_trie_matching(text, trie):
            all_pos.append(str(k))
        text = text[1:]
        k += 1

    print(" ".join(all_pos))

"""
Code Challenge: Implement TrieMatching to solve the Multiple Pattern Matching Problem.

Input: A string Text and a collection of strings Patterns.
Output: All starting positions in Text where a string from Patterns appears as a substring.
"""
def prefix_trie_matching(string, trie):
    k = 0
    node = trie
    while True:
        if len(node.children) == 0:
            return True
        elif k >= len(string):
            return False
        elif string[k] in node.children.keys():
            node = node.children[string[k]]
            k += 1
        else:
            return False

def dfs(node, parent_id):
    print(f"{parent_id}->{node.id}:{node.char}")
    for n in node.children.values():
        dfs(n, node.id)

"""
# Trie Construction
root = Node(0, '0')
with open('trie_matching.txt', 'r') as f:
    patterns = f.read().splitlines()
    for pattern in patterns:
        insert(pattern, root)
        
for n in root.children.values():
    dfs(n, 0)
"""

# Prefix Trie Matching
counter = 1
root = Node(0, '0')
with open('trie_matching.txt', 'r') as f:
    patterns = f.read().splitlines()
    text = patterns[0]
    for pattern in patterns[1:]:
        insert(pattern, root)

    trie_matching(text, root)
