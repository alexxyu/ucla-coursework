import sys

class Node:
    def __init__(self, char):
        self.char = char
        self.children = dict()

def insert(string, root):
    curr = root
    for c in string:
        if c not in curr.children.keys():
            curr.children[c] = Node(c)
        curr = curr.children[c]

def construct_suffix_trie(text, trie):
    for i in range(len(text)):
        insert(text[i:], trie)

def dfs(node):
    for child in node.children.values():
        print(f"{node.char}->{child.char}")
        dfs(child)

def get_path_to_node(tree, node, path):
    # Returns the path from root to node
    for child in tree.children.values():
        if child == node:
            path.insert(0, child.char)
            return True
        if get_path_to_node(child, node, path):
            path.insert(0, child.char)
            return True

    return False

"""
Code Challenge: Solve the Suffix Tree Construction Problem.

Input: A string Text.
Output: The edge labels of SuffixTree(Text). You may return these strings in any order.
"""
def suffix_trie_to_tree(trie):
    for child in trie.children.values():
        suffix_trie_to_tree(child)

    if len(trie.children.keys()) == 1:
        child = list(trie.children.values())[0]
        trie.char += child.char
        trie.children = child.children

"""
Longest Repeat Problem: Find the longest repeat in a string.

Input: A string Text.
Output: A longest substring of Text that appears in Text more than once.
"""
def longest_repeat(tree):
    # Approach: Check internal nodes
    def get_internal_nodes(node, depth, candidates):
        for n in node.children.values():
            if len(n.children) != 0:
                candidates.append((n, depth+1))
                get_internal_nodes(n, depth+1, candidates)

    candidates = []
    get_internal_nodes(tree, 0, candidates)
    candidates.sort(key=lambda x: x[1])

    longest_rep_node, max_depth = candidates[-1]
    longest_repeat_substr = ""

    i = len(candidates)-1
    while i >= 0:
        path = []
        curr_node, _ = candidates[i]
        get_path_to_node(tree, curr_node, path)
        curr_substr = ''.join(path)

        if len(curr_substr) > len(longest_repeat_substr):
            longest_repeat_substr = curr_substr

        i -= 1

    return longest_repeat_substr

"""
Longest Shared Substring Problem: Find the longest substring shared by two strings.

Input: Strings Text1 and Text2.
Output: The longest substring that occurs in both Text1 and Text2.
"""
def longest_shared_substring_dp(text1, text2):
    dp = [[0 for _ in range(len(text1)+1)] for _ in range(len(text2)+1)]

    result = ""
    for i in range(1, len(text1)+1):
        for j in range(1, len(text2)+1):
            if text1[i-1] == text2[j-1]:
                dp[i][j] = dp[i-1][j-1] + 1
                substr = text1[i - dp[i][j]:i]
                if len(substr) > len(result):
                    result = substr

    return result

"""
Shortest Non-Shared Substring Problem: Find the shortest substring of one string that does not appear in another string.

Input: Strings Text1 and Text2.
Output: The shortest substring of Text1 that does not appear in Text2.
"""
def shortest_nonshared_substring(tree1, tree2, text2):
    # Approach: Find nodes in text1's suffix tree that do not show up in text2's suffix tree
    def get_all_values(tree, s):
        for child in tree.children.values():
            s.add(child.char)
            get_all_values(child, s)

    tree2_vals = set()
    get_all_values(tree2, tree2_vals)

    def search_tree(tree, candidates):
        for child in tree.children.values():
            if child.char not in tree2_vals:
                candidates.append(child)
            else:
                search_tree(child, candidates)
    
    candidates = []
    search_tree(tree1, candidates)

    shortest_substr = None
    for c in candidates:
        path = []
        get_path_to_node(tree1, c, path)
        
        curr = ''.join(path)
        if shortest_substr is None or (len(curr) <= len(shortest_substr) and curr not in text2):
            shortest_substr = curr

    temp = shortest_substr
    while temp != "":
        temp = temp[:-1]
        if temp not in text2:
            shortest_substr = temp
        else:
            break
    return shortest_substr

# with open('suffix_tree.txt', 'r') as f:
#     text1, text2 = f.read().splitlines()
#     root1, root2 = Node(''), Node('')
#     construct_suffix_trie(text1, root1)
#     construct_suffix_trie(text2, root2)

#     suffix_trie_to_tree(root1)
#     suffix_trie_to_tree(root2)

#     print(shortest_nonshared_substring(root1, root2, text2))

with open('suffix_tree.txt', 'r') as f:
    root = Node('')
    text = f.read().splitlines()[0]

    sys.setrecursionlimit(1500)
    construct_suffix_trie(text+"$", root)
    suffix_trie_to_tree(root)

    print(longest_repeat(root))
