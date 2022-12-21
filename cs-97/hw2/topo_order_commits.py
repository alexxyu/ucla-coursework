import os
import sys
import zlib

class CommitNode:
    def __init__(self, commit_hash):
        """
        :type commit_hash: str
        """
        self.commit_hash = commit_hash
        self.parents = set()
        self.copy_parents = set()
        self.children = set()
        self.branches = set()

def build_graph(root_commits, commits):
    """
    Builds DAG of commit nodes from each available branch
    """

    # Iterate through each branch in the directory and backtrack through commit nodes
    local_branches = os.listdir('.git/refs/heads/')
    for branch in local_branches:
        if os.path.isdir('.git/refs/heads/%s' % branch):
            branch += '/branch'
        with open('.git/refs/heads/%s' % branch) as branch_file:
            
            branch_head = branch_file.read().replace('\n','') 

            # Add all parent-child node relationships for current branch
            push_list = [branch_head]
            while len(push_list) > 0:
                hash = push_list.pop(0)

                # Get or create new child node if it does not already exist
                if hash in commits:
                    child = commits[hash]
                else:
                    child = CommitNode(hash)
                    commits[hash] = child

                # Add branch labels for branch heads
                if hash is branch_head:
                    child.branches.add(branch)
                
                # Get commit data for current node
                compressed_object = open('.git/objects/%s/%s' % (hash[:2], hash[2:]), 'rb')
                decompressed_data = zlib.decompress(compressed_object.read()).decode()
                compressed_object.close()

                if 'parent' not in decompressed_data:
                    # This is a leaf node because it has no parent
                    root_commits.add(child)
                    continue

                # Establish relationship between current child node and each parent
                for parent_commit in decompressed_data.split('parent')[1:]:
                    parent_commit = parent_commit.split()[0]

                    # Get or create new parent node if it does not already exist
                    if parent_commit in commits:
                        parent = commits[parent_commit]
                    else:
                        parent = CommitNode(parent_commit)
                        commits[parent_commit] = parent

                    child.parents.add(parent)
                    child.copy_parents.add(parent)
                    parent.children.add(child)
                    push_list.append(parent_commit)

def topological_sort(roots, commits):
    """
    Topologically sorts commit nodes using Kahn's Algorithm.
    """

    result = []
    roots = list(roots)

    while len(roots) > 0:
        node = roots.pop()
        result.append(node.commit_hash)

        for child in node.children:
            child.parents.remove(node)

            if len(child.parents) == 0:
                roots.append(child)

    return result

def print_ordering(sorted, commits):
    """
    Formats and prints out the topologically sorted commits line-by-line.
    """

    sorted.reverse()
    just_printed_space = False

    for i in range(len(sorted)):
        curr = sorted[i]

        # Print sticky start
        if just_printed_space:
            sticky_start = [p.commit_hash for p in list(commits[curr].children)]
            sticky_start.sort()
            print(f"={' '.join(sticky_start)}")
            just_printed_space = False
        
        # Print current commit (with branches if a branch head)
        print(f"{curr} {' '.join(list(commits[curr].branches))}")
        
        # Print sticky end
        if i < len(sorted) - 1 and commits[curr] not in commits[sorted[i+1]].children:
            sticky_end = [p.commit_hash for p in list(commits[curr].copy_parents)]
            sticky_end.sort()
            print(f"{' '.join(sticky_end)}=")
            print()
            just_printed_space = True
        else:
            just_printed_space = False

def main():
    while os.path.abspath(os.curdir) != '/':
        if os.path.exists('.git'):
            break
        os.chdir('..')

    if not os.path.exists('.git'):
        sys.stderr.write('Not inside a Git repository.\n')
        exit(1)

    commits = {}
    root_commits = set()
    build_graph(root_commits, commits)

    print_ordering(topological_sort(root_commits, commits), commits)

if __name__== "__main__":
    main()
