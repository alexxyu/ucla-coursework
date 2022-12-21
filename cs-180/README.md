# CS 180

Notes for CS 180 Fall 2020 with Majid Sarrafzadeh

## Representative Problems

###  Famous Problem

**Problem description:** Suppose you have a set of people. A famous person in that set is defined as a person who does not know anyone else in that set, and everyone else in the set knows this person. Given a set of people, provide an algorithm to find whether there exists a famous person in this set.

**Algorithm:**

```pseudocode
While there exists more than one person in the set,

- Take an arbitrary pair of people, p_1 and p_2.

-	Ask whether p_1 knows p_2.

-	If p_1 knows p_2, remove p_1 from the set

-	If p_1 does not know p_2, remove p_2 from the set

End

Take the last person p_x from the set

Return true iff everyone else knows p_x, and p_x does not know anyone else; otherwise, false
```

**Proof of correctness:** Suppose that our algorithm is not correct.

* Suppose that our algorithm returns that there is no famous person, but there is.
  * Suppose the famous person is *p*.
  * During the while loop, suppose p_2 = p. Then, since *p* is famous, p_1 must answer that s/he knows p_1, and p_1 is removed from the set.
  * During the while loop, suppose p_1 = p. Then, since *p* is famous, *p* must answer that s/he does not know p_2, and p_2 is removed from the set.
  * We can conclude that p is guaranteed not to be removed from the set, and s/he must be the last person remaining in the set by the end of the while loop.
  * Then, the algorithm checks whether everyone knows p and whether p knows no one. Since it would have found both of these to be true, it would have returned true, which contradicts the supposition that our algorithm returns that there is no famous person.
* Suppose that our algorithm returns that there is a famous person, but there isn't. However, our algorithm returned true because upon checking p', it found that everyone knows p', and p' knows no one. By definition, p' must be a famous person.

**Time Complexity:** During each iteration of the while loop, our algorithm removes one person from the set. The while loop iterates until there is exactly one person in the set. Provided that each iteration is done in constant time, the algorithm takes *O(n)* time in total for this while loop. Then, the algorithm has to check whether everyone knows p', which takes *O(n)* time to ask the other *n-1* people. Likewise, the algorithm has to check whether p' does not know everyone, which takes *O(n)* time to ask p' n-1 times. Thus, our algorithm runs in *O(3n)=O(n)* time.

### Stable Matching Problem

**Problem description:** Suppose there are *n* men and *n* women. We want to form *n* pairs, each with one man and one woman. No man and no woman can be in two pairs at once. We call this a perfect matching. Each person has a preference list in which s/he ranks all *n* people of the opposite gender in order from greatest preference to lowest preference. No two people can have the same preference level in the same preference list.

A perfect matching is unstable if there exists a man *m* and a woman *w* such that *m* and *w* prefer each other over their current partners. We want to create a perfect matching with no instability.

**Algorithm:**

```pseudocode
While there exists a free man who has not proposed to every woman,

- Choose such a man m

- Have the man ask the woman with the greatest preference whom he hasn't proposed to yet, w

- If w is free, tentatively accept m's proposal

- If w already has a partner, accept m's proposal only if he has greater preference in w's list; w's partner before m now becomes free

End
```

At the end, the current pairings become final, and the matching is returned.

**Proof of correctness:** Suppose the matching returned by our algorithm contains an instability. Specifically, there exists men *m*, *m'* and women *w*, *w'* such that *m* is paired with *w*, *m'* is paired with *w'*, and *m* and *w'* prefer each other over their respective partners.

If so, we ask ourselves whether *m* proposed to *w'* at some point during the execution of our algorithm.

If yes, then that means that at some point for *w'* not to end up with *m*, a man *m''* proposed to *w'*, and *w'* agreed to his proposal because *m''* is preferred over *m*. Either *m''* = *m'* or *m'* has greater preference over *m''* using the same logic. Either way, *m'* has greater preference over *m*, contradicting the supposition that *w'* prefers *m* over *m'*.

If no, then that means that *m* proposed to *w* first, and this implies that *w* has greater preference over *w'*. This contradicts the supposition that *m* prefers *w'* over *w*.

By proof by contradiction, there must not exist an instability in any perfect matching returned by our algorithm.

**Time Complexity:** For each of the *m* men, we propose to each of the *w* women. Each woman checks whether she is free and her preference list in constant time. Since *m=w*, the algorithm runs in *O(n^2)* time.

## Graphs

### Breadth-First Search

**Problem description:** The level assigned to a vertex *v* is equal to the distance from that vertex to the source in the original graph *G*.

**Proof:** The level assigned to a vertex *v* is equal to the distance from that vertex to the source in the original graph *G*.

* Suppose that there is a shorter path *P* of length *j* from the source to *v*. Suppose that *v* is at level *i*.
* *p_1* must be at level 1 because it is a neighbor of the source.
* *p_i* must be at level *i* or less.
* Because *P* is of length *j*, *v* must be at level *j* or less. However, *v* is by definition at level *i*, so there cannot be a shorter path *P*.

By proof by contradiction, the level assigned to a vertex *v* is equal to the distance from that vertex to the source in the original graph *G*.

**Time Complexity:** For each of the *n* nodes, we add each of its neighbors and mark it visited. There are *e_n* neighbors per *n* and a constant operation per *n*. Over all *n*, this is equivalent to visiting all nodes and edges, so our algorithm runs in *O(n + e)* time.

### Topological Sorting

**Problem Description:** Suppose we have a directed acyclic graph *G*. We want to find a sorting of nodes such that for any given node, every one of its incoming nodes shows up first in the output.

**Algorithm:**

```pseudocode
Add all sources to a queue

While there exists a source in the queue,

-	Remove the source

-	Output the source

-	Delete the source and every outgoing edge of the source

-	Add any new sources to the queue

End
```

**Proof of correctness:**

* Let the base case be the first node being output. It is in the correct position as it is a source, and a source has no pre-requisites.
* As the inductive step, let us assume that the algorithm correctly outputs the first *i* nodes.
* Node *i+1* is in a valid position, as it is now a source in the graph, meaning it either never had any prerequisite nodes, or its prerequisite nodes have already been visited and removed from the graph.

**Time Complexity:** To find all sources initially, our algorithm takes *O(e+n)* time because we must calculate every node's indegree. For each of the *n* nodes, we then have to delete every outgoing edge and add any new sources to the queue. This is another *O(e+n)* units of time. Our algorithm runs in *O(e+n)* time in total.

### 2-Coloring Problem

**Problem description:** We are given a graph *G* with no odd cycles, and we want to color each node either blue or red such that no two adjacent nodes have the same color.

**Algorithm:** Conduct a BFS starting from node *s*. Color all nodes within each level a different color, and alternate colors between levels.

**Proof of correctness:** Suppose that our algorithm does not work. That is, there exist two adjacent nodes that have the same color. Then, since nodes between levels are guaranteed to have different colors by our algorithm, this implies that the two nodes are at the same level. It follows that there exists an edge between these two nodes since they are adjacent.

Suppose that the two nodes are both at level *m*. Then, their distance from *s* is each *m*. We can create a cycle from *s* to one of the nodes to the other node back to *s*. The length of this cycle is *m+1+m=2m+1*. Since *m* is an integer, *2m+1* is an odd integer, meaning this cycle is odd. This contradicts the supposition that *G* has no odd cycles. Thus, there cannot exist two adjacent nodes with the same color through our algorithm.

## Greedy Algorithms

### Scheduling Problem

**Problem description:** We have a set of *n* tasks, each with start and end times. We want to find a subset of these tasks such that none of them overlap, and it contains the most amount of tasks possible. A task with some end time does not overlap with another task that has the time as its start time.

**Algorithm:**

```pseudocode
Scan from left to right.

While the set of tasks is not empty,

- Take the task from the set that ends first among all tasks in the set and add to solution set.

- Then remove any tasks that overlap with this task from the set.

End

Return solution set
```

**Proof of correctness:** Suppose there is a subset, S', that is better than the one returned by our algorithm, S. That is, that the subset contains more tasks than ours.

We can compare each task in S' to each task in S. The first task that differs, we can call t_i' and t_i. We can substitute t_i for t_i' without worrying about any overlap because the end time of t_i must be earlier than the end time of t'_i based on our algorithm's heuristic.

We can continue substituting any differing task in S' with the corresponding one in S. If there are any remaining tasks in S', then they would have surely been added to S by our algorithm since there is no overlap. Therefore, we have reached a contradiction.

There cannot exist a subset S' that has more tasks than S.

**Time Complexity:** If our list of tasks is sorted by their end time in ascending order initially, then we linearly scan through our tasks. For each of the *n* tasks, we either add them to our set or remove them if they conflict with another, which can each be done in constant time. Thus, our algorithm runs in *O(n)* time.

### Shortest Path in a Weighted Graph (No Negative Weights)

**Problem description:** We are given a positive weighted directed graph. Find the shortest path between node *s* and some other node *t* in this graph.

**Algorithm:** (Dijkstra's Algorithm)

```pseudocode
Finalize s with a shortest path of 0.

Initialize all other nodes with a shortest path length of infinity

Update all neighbors (see below what this entails)

While there is a node that is not finalized,

-	Retrieve such a node n with the shortest path

-	Finalize n

-	Update the distances to all neighbors of n

End
```

What updating neighbors entails: For each neighbor *n'* of *n*, set its shortest path length to the minimum between its current shortest path length and the shortest path length of *n* + (the weight of the edge (*n*, *n'*)).

**Proof of correctness:**

* Let the base case be the shortest path from *s* to itself. Our algorithm initializes the shortest path for *s* to be 0, which is by definition  the length of the shortest path from a node to itself.
* As the inductive step, assume our algorithm correctly finds the shortest path between *s* and node *p*.
* Our algorithm now retrieves node *q* as the non-finalized node with the shortest path length. All other non-finalized nodes have shortest path lengths ≥ *q*'s shortest path length. Then, any path from those non-finalized nodes to *q* will be greater than the current shortest path for *q* because all weights in the graph are positive, and so any other path will have to take on more weight. Thus, we can finalize *q* as having the shortest path length.

By proof by induction, we have proven that our algorithm correctly finds the shortest path from *s* to any node *n* in our graph.

**Time complexity:** We visit each edge in the process of adding and updating each of the current node's neighbors. For each of the *e* edges, we also have to retrieve the non-finalized node with the shortest path. If we use a heap, then this retrieval can be done in *O(log n)* time per retrieval. Thus, the algorithm runs in *O(e\*log n)*.

Note, we can also assign the time cost to the nodes as follows. While there is a non-finalized node, we take the non-finalized node with the current shortest path among all nodes, which is *O(n)* time per node visit. We then update all neighbors of *n*, which is another *O(n)* time per node visit. Since our algorithm will check all *n* nodes in the graph, it runs in *O(n(n+n))=O(n^2)* time.

### Minimum Spanning Tree

**Problem description:** We have an undirected weighted graph *G*, and we have to find a set of edges such that this set forms a spanning tree with the lowest possible weight. A spanning tree is a tree on *G* that connects all vertices in *G*.

**Algorithm 1:** (Vertex-centric Prim's)

```pseudocode
Create cost dict of node to infinity

Pick node n_1 and set cost(n_1) = 0

Initialize min heap H of the node to cost and add n_1

While H is not empty,

-	Retrieve v from H

-	For each edge (v, v_p) adjacent to v,

		- If w(v, v_p) < cost(v_p):

				- Add edge (v, v_p) to the MST

				- cost(v_p) = w(v, v_p)

				- Update heap with new v_p cost

End

Return the MST
```

**Time Complexity (Prim's):** This algorithm takes *O(elog n)* time. We can maintain a heap structure to keep track of the node with min cost. We will iterate through the while loop *n* times over all nodes in *G*. For each iteration, we retrieve a node *v* from the heap, which takes *O(log n)* to heapify. Then, we visit each edge adjacent to *v*, and potentially update the heap with the new cost of *v'*, which takes *O(log n)* time to update the heap. Since we visit every adjacent edge to every node in *G*, we will ultimately check every edge in *G*, and for each of the *e* edges, we will have to update the heap. Thus, the algorithm takes *O(elog n)* in total.

**Algorithm 2:** (Edge-centric Kruskal's)

```pseudocode
Initialize a set of n nodes, one for each of the nodes in G. Each node initially has a null pointer.

While there are fewer than n-1 edges in the MST,

-	From the set of unchecked edges, take the edge e=(n_1, n_2) with the lowest weight

-	Check if n_1 and n_2 share the same root node

-	If yes, don't add e to the MST

-	Otherwise, add e to the MST

-	Then, take the shorter tree between n_1 and n_2 and set the pointer of its root node to the root node of the other tree

-	Mark e as checked

End

Return the MST
```

**Proof of MST Theorem:**

Suppose that *e* is the minimum-weight edge among all edges between partition 1 and partition 2 (as in, the edges connect a node in partition 1 to a node in partition 2). Then, *e=(x,y)* must be in the MST.

* Suppose that *e* is not in the MST.
* Then, we can add *e* to the MST, which will create a cycle because there will now be *n* edges in the tree, which implies there is a cycle.
* In particular, there must exist an edge *e_x=(w,z)* in the MST such that there is a path from *x* to *w* and another path from *z* to *y*.
* Then, we can destroy the cycle by removing edge *e_x*.
* The weight of *e* must be less than the weight of *e_x* because we established that *e* is the minimum-weight edge between the partitions. Note that *e_x* is also between the two partitions.
* This implies that the total weight of the tree with *e* is less than the total weight of the original MST.
* Then, the original MST cannot be an MST since there exists a spanning tree with lower weight.
* Thus, by proof by contradiction, *e* must be in the MST.

**Time complexity (Kruskal's):** This algorithm takes *O(elog e)*. Initially, we sort the edges by their weight, which takes *O(elog e)* time. Then, for each of the *e* edges, we have to check whether the two nodes connected by the edge share the same root node, which takes *O(log e)* time to traverse the tree "upwards". Thus, the algorithm takes *O(elog e + elog e) = O(elog e)* time in total.

### Clustering

**Problem description:** We are given a graph *G*. We want to form *k* clusters -- as in, *k* acyclic subgraphs wherein the subgraphs are not connected to each other -- in the following manner. Let *d* be the minimum distance between any two clusters. We want to form the clusters such that *d* is maximized.

**Algorithm:** Run Kruskal's algorithm, but stop execution when there are exactly *k* clusters.

**Proof of correctness:**

* Suppose there is an optimal clustering that has a greater *d* value than ours. Let the optimal clustering *S'=C1 C2...Ck*. Let our clustering *S=C1 C2...Ck*.
* Because *S'≠S*, there must exist a cluster *Ci* in *S* such that its nodes are in at least two clusters, *C'd* and *C'f*, in *S'*. Let *p_i* and *p_j* be nodes in *Ci*, with *p_i* in *C'd* and *p_j* in *C'f*.
* Then, there must also be some edge *e_x* that lies on the path from *p_i* to *p_j* in *Ci* that is not included in *S'* since *p_i* and *p_j* are in different clusters.
* It follows that *e_x* is the shortest distance between *C'd* and *C'f*, so *d'*, the minimum distance between any two clusters in *S'*, has to be less than or equal to *d'=e_x*.
* In *S*, since *e_x* was evaluated and checked by Kruskal's, there must exist an edge *e_y* that was the minimum-weight edge that was never checked by Kruskal's. Thus, *e_x ≤ e_y*, and *e_y* is the shortest distance between any two clusters in *S*, so *d=e_y*.
* Thus, since *e_x ≤ e_y*, *d' ≤ d*. Then, the optimal clustering has a minimum cluster distance that cannot exceed ours, so there cannot exist any better clustering than ours.

**Time complexity:** The algorithm runs in *O(e log e)* time because we are simply running Kruskal's algorithm.

## Divide and Conquer

### Merge Sorting

**Problem description:** Given an unsorted array *A* of *n* elements, provide an algorithm that sorts this array in nondecreasing order in *O(n log n)* time.

**Algorithm:**

```pseudocode
Recursively divide the current array into halves

When the size of the current array = 1, return since an array of one element is by definition sorted

Merge the two sorted arrays in the following fashion:

- Set a ptr p_1 and a ptr p_2 to the first indices of A_1 and A_2 respectively, the two halves of the current array

- If *p_1 < *p_2, add *p_1 to the merged array and increment p_1

- Otherwise, add *p_2 to the merged array and increment p_2

Return the merged array
```

**Proof of correctness:** We will focus on the proof of correctness for the merge operation.

* Let the base case be the empty merged list. This is technically sorted.
* Suppose that we've correctly merged the two arrays up to index *k* in the list. Now we will show that we put the correct element at index *k+1*.
* Let the pointers' dereferenced values be *v1* and *v2*. We get the smaller of the two values. WIthout loss of generality, suppose that *v1* is the smaller value.
* Then, since the two arrays are sorted already, we know that *v1* is less than all the other elements after it in its array, and since *v2* is less than all the other elements after it in its array, *v1* is also less than all the elements after *v2*.
* Then, *v1* must be the next element in the list at index *k+1*, which is exactly what our algorithm does.

**Time complexity:** To find the time complexity of merge sort, we can solve the following recurrence relation. Sorting the two halves of the array takes 2*T(n/2), and merging them together takes cn time.

T(n) = 2T(n/2) + cn

T(n) = 2*(2T(n/4) + cn/2) + cn = 2^2 T(n/2^2) + 2 cn

T(n) = 2^2 * (2T(n/8) + cn/4) + 2 cn = 2^3 T(n/2^3) + 3 cn

T(n) = 2^k * T(n/2^k) + k cn

T(1) = O(1), n/2^k = 1 => k = log n



T(n) = 2^(log n) * O(1) + cn log n

T(n) = n * O(1) + cn log n = O(n) + cn log n

T(n) = O(n log n)

### Inversion Count

**Problem description:** We are given an array *A* of *n* integers. An inversion is defined as a pair of indices *(i, j)* such that *i < j* and *a_i > a_j*. Give an algorithm that outputs the number of unique inversions in *A*.

**Algorithm:**

```pseudocode
Run merge sort but with an extra step in the merge operation with sorted left subarray L_1 and sorted right subarray L_2 as follows:

Keep an inversion counter k.

At every step in merging the array, let x from L_1 and y from L_2 be the two elements being compared.

If y < x, then increment k by one + the number of elements after x in L_1.
```

**Proof of correctness:**

* *L_1* is the left subarray, and *L_2* is the right subarray.
* If *y < x*, this means that an element on the right is less than an element on the left, so there is at least one inversion consisting of *(x, y)*.
* Then, since every element after *x* is also greater than *x* since *L_1* is sorted, and *x > y*, it follows that every element after *x* is also greater than *y*.
* Thus for every element *x'* after *x* in *L_1*, there is an inversion consisting of *(x', y)*, so we increment *k* by one for *(x,y)* plus one for each *(x', y)*.

**Time complexity:** We have the same recurrence relation as in merge sort because we've only added constant operations for each merge operation. Thus, the recurrence relation is still T(n) = 2T(n/2) + O(n), which simplifies to T(n) = O(n log n) as in merge sort.

### Closest Pair of Points

**Problem description:** Given a set of *n* cartesian points, provide an algorithm that finds the Euclidean distance between the closest pair of points in the set.

**Algorithm:**

```pseudocode
Sort the list of points by their x-coordinates, called P_x
Sort the list of points by their y-coordinates, called P_y

Recursively divide the list of points into halves

Suppose we have two sublists L1 and L2, each of which have minimum pair distances d1 and d2

Let d = min(d1, d2)

Define x to be the horizontal midpoint between the rightmost point in L1 and the leftmost point in L2

Let Q be the set of all points in L1 and L2 whose x-coordinates are within d of x

Sort Q by y and call it Q_y

For each point p in Q_y, check the distance between p and each of the next 15 points in Q_y

Let d be the min between itself and the distances checked in the for-loop

Return d as the minimum distance in the merged solution
```

**Proof of correctness:**

**Time complexity:** O(n log n)

## Dynamic Programming

### Maximum Weighted Subsets of Nonoverlapping Intervals

**Problem description:** We are given a list of *n* intervals. Each interval *i* has weight *w_i*, start time *s_i*, and end time *f_i*. Provide an algorithm that returns the total weight of the set of non-overlapping intervals with minimum total weight. We assume that the intervals have unique start and end times.

**Algorithm:**

```
Sort the list of intervals by end time

Initialize a 1-D array A of all 0's

for each interval i,
	A[f_i] = min(A[f_i-1], A[s_i-1] + w_i)

Return A[f_n]
```

**Proof of correctness:**

For a given *i*, OPT(f_i) = OPT(s_i -1) + *w_i* if *i* is in the optimal solution or OPT(f_i) = OPT(f_i - 1) if *i* is not in the optimal solution. Thus, OPT(f_i) = min(OPT(s_i-1)+w_i, OPT(f_i-1)).

**Time complexity:** The algorithm runs in O(n log n) time.

### Knapsack Problem

**Problem description:** We are given a knapsack of size *S* and a list of *n* items, each with value *v_i* and size *s_i*. We want to place as many items such that the sum of their individual sizes is no more than *S*, and the total value of all the items placed is maximized. Duplicates of the same item are allowed. Provide an algorithm that returns the maximum value of items that can be placed in the knapsack.

**Algorithm:**

```pseudocode
Initialize a 2-D array A of all zeroes

for i=1 to n,
	for j=1 to S,
		A[i][j] = max(A[i-1][j], A[i][j-s_i]+v_i)

Return A[n][S]
```

**Proof of correctness:** Each entry A_ij represents a knapsack of size j and the possibility of adding item i to the knapsack. For such any such j, we have two options:

1. We add the item to the knapsack. Then, the maximum value of the knapsack of size j is the maximum value of the knapsack of the size that is j-s_i plus the value of item i.
2. We don't add the item to the knapsack. Then, the new value of the knapsack is the value of the knapsack of same size with the possibility of adding up to item i-1 to the knapsack.

**Time complexity:** O(nS).

### Segmented Least-Squares Fit

**Problem description:** We are given a list of *n* points and a constant real number *c*. We are also given a function *lsq_fit* that computes the best fit line (slope *a* and intercept *b*) for some input of points, and a function *lsq_error* that computes the error of the best fit line given some input of points, a, and b. Provide an algorithm that returns the optimal number of segmented lines k, with optimality defined as minimizing the sum of total error of the segmented lines over their corresponding points and c*k.

Note that *lsq_fit* and *lsq_error* each take O(n) time given n points as input.

**Algorithm:**

```pseudocode
for 1 <= i <= n,
	for 1 <= j <= i,
		a, b = lsq_fit(points j through i)
		error_x = E[j-1]
		error_y = lsq_error(points j through i, a, b)

		error_z = error_x + error_y + (L[j-1]+1) * c
		if error_z < E[i],
			E[i] = error_z
			L[j] = L[j-1] + 1

return L[n]
```

**Proof of correctness:** Suppose we know the optimal solution for points 1 through i-1. For point i, we can do the following:

* Suppose we take any point k such that 1 <= k <= i. Then, we form a best fit line through points k to i.
* Then, the total error is the error from 1 to k-1 plus the error from k to i plus the c*L term, where L is the number of optimal lines through k-1, which we know by the inductive step, plus one for the line through points k to i.
* Mathematically, this is error(1, k-1) + error(k, i) + c*(L(1, k-1) + 1).
* Thus, the optimal solution for points 1 to i is given by min(error(1, k-1) + error(k, i) + c*(L(1, k-1) +1)) for all valid values of k, which is 1 through i.

**Time complexity:** O(n^3).

### RNA Sequence Matching

**Problem description:** We are given a sequence of RNA bases (some string of A, U, C, and G characters) of length *n*. Provide an algorithm that calculates the maximum number of pairings that meet the following constraints:

1. A and U can pair only with each other, and C and G can pair only with each other.
2. A base can be part of at most one pairing.
3. Pairings cannot "cross" -- if base i pairs with base j, there cannot be a pairing of base k for i < k < j and base m for m > j.
4. Pairings cannot be too close together -- if base i pairs with base j, then i < j-4.

**Algorithm:**

```pseudocode
for 1 <= i <= n,
	for 1 <= j <= i,
		A[j][i] = max(A[j][i], A[j][i-1])
		for j <= k <= i-5,
			if B_k can pair with B_i,
				A[j][i] = max(A[j][i], A[1][k-1] + A[k+1][i-1] + 1)

return A[1][n]
```

**Proof of correctness:** Each entry A_ji means that there are a maximum of x pairings that can be made between base j through i. Then, for each base k for j <= k <= i, we have one of two choices:

1. If valid, we form a pairing between base k and i. Then, the maximum number of pairings with this arrangement is the maximum number of pairings between bases 1 and k-1 plus the maximum number of pairings between bases k+1 and i-1 plus one for the new pairing created.

2. We do not form a pairing between base k and i. Then, the maximum number of pairings is still the maximum number of pairings between base j through i-1.

Note that the matching satisfies the constraints. In particular, let us prove that the pairings do not cross. In particular, whenever we form a pairing between some base k and base i, we take the maximum number of pairings between bases 1 through k-1 and bases k+1 and i-1. Within each of these subproblems, the pairings must be between two bases within these problems. Thus, there is no "crossing-over" with the pairing B_k-B_i.

**Time complexity:** O(n^3).

### Shortest Paths in a Weighted graph (With Negative Weights)

**Problem description:** We are given a directed weighted graph G. The weights of the edges can be negative. Provide an algortihm that finds the shortest path between a node s in G to some other node u in G.

**Algorithm:**

```pseudocode
Initialize a 2-D array A containing infinite values
Set A[0][0] = 0 (Assume n_0 = s)

for 1 <= L <= n-1,
	for 1 <= i <= n,
		A[L][i] = A[L-1][i]
		foreach neighbor n_j of n_i,
			A[L][i] = min(A[L][i], A[L-1][j] + w_ji)

return A[n-1][u]
```

**Proof of correctness:** Each entry A_ij represents the shortest path for a path of length no greater than i, going from the source node s to the current node n_j. Let us assume that we know the shortest path to n_j of length less than i.

To get to the current node n_j through a path of length i, we clearly must get to one of its neighbors through a path i-1 first. Thus, the minimum distance to get to n_j is the minimum among all paths with length less than i as well as all paths that go through one of n_j's neighbors. The latter is D(s, n_k, i-1) + w_kj for all n_k that are neighbors of n_j.

**Time complexity:** O(n^2)

### Sequence Alignment

**Problem description:** We are given two sequences, one of length m and the other of length n. An alignment is defined as a pairing of the same characters between the two sequences. In a valid matching, no two alignments can cross. Provide an algorithm that returns the maximum number of alignments possible between the two sequences.

**Algorithm:** Suppose the two sequences are L and S.

```pseudocode
Initialize a 2-D array A containing zeroes

for 1 <= i <= m,
	for 1 <= j <= n,
		A[i][j] = max(A[i-1][j], A[i][j-1])
		if L_i equals L_s,
			A[i][j] = A[i-1][j-1] + 1

Return A[m][n]
```

**Proof of correctness:** Each entry A_ij represents the maximum number of alignments up to the ith character in L and the jth character in S.

Suppose we know the optimal solution up to the (i-1)th character in L and the jth character in S. Then, there are two possibilities:

1. L_i = S_j. Then, it is optimal to align L_i with S_j. By way of contradiction, suppose not. Then, without loss of generality, let us assume that the optimal solution pairs S_j with L_k for k < i. Then, we can replace that alignment and do no worse than the optimal solution. Thus, we should align L_i with S_j optimally. The total number of alignments up to this point is therefore the number of alignments up to the (i-1)th character in L and the (j-1)th character in S plus one.

2. L_i ≠ S_j. Then, there are two options:

   a. L_i may be aligned with some character before S_j, while S_j is left unaligned. Then, the total number of alignments is the number of alignments up to the ith character in L and the (j-1)th character in S.

   b. S_j may be aligned with some character before L_i, while L_i is left unaligned. Then, the total number of alignments is the number of alignments up to the (i-1)th character in L and the jth character in S.

   The overall total number of alignments should be the maximum of these two options.

**Time complexity:** O(n^2).

## Network Flow

### General Network Flow

**Problem description:** We are given a directed graph G = (V, E). Each edge is given a positive integer capacity value. A flow f between nodes s and t is defined as a set of integers assigned to each edge such that

1. f(e) ≤ c(e) for all e
2. f_in(n) = f_out(n) for all n other than s and t
3. f_out(s) = f_in(t)

The size of the flow |f| is defined as the sum of the flow integers on the edges leaving s. Provide an algorithm that finds the max flow -- the flow that maximizes |f|.

**Algorithm:**

A saturated edge is defined as an edge whose flow is equal to its capacity.

```pseudocode
foreach edge e=(u,v) in G,
	add backedge (v,u) with capacity 0 to G

initialize flow counter f = 0
while there exists a path of non-saturated edges from s to t in G,
	find such a path P via BFS

	increment f by 1
	foreach edge e=(u,v) in P,
		if e is a backedge,
			decrement flow through (v,u) by 1
			decrement capacity of e by 1
		else,
			increment flow through e by 1
			increment backedge (v,u) capacity by 1

return f
```

**Proof of correctness:** We will show that the following three statements are equivalent.

1. f is a maxflow of G.
2. The residual graph G_f contains no augmenting path (a path from s to t).
3. A flow f is equal to the capacity of some cut c of G.

A cut is defined as a partition of nodes + s into one set, and all other nodes + t into the other set. The capacity of the cut is defined as the sum of the capacities of the edges from the partition with s to the partition with t.

We will prove that 1 implies 2, 2 implies 3, and 3 implies 1.

**1. If f is a maxflow of G, then the residual graph G_f contains no augmenting path.**

* By way of contradiction, suppose G_f contains an augmenting path.
* Then, we can push one more unit of flow through this augmenting path, creating a flow with greater size.
* This contradicts f being the maxflow.

By proof by contradiction, if f is a maxflow of G, then G_f contains no augmenting path.

**2. If the residual graph G_f contains no augmenting path, |f| is equal to the capacity of some cut c of G.**

* Since G_f contains no augmenting path, there must exist at least one saturated edge.
* Let us remove all saturated edges from G_f.
* Then, we are able to form a cut of G_f by placing all nodes connected to s including s itself into one partition, and all other nodes including t in the other partition. Note that s and t are now disconnected.
* Now, take the capacity c of this cut.
* Suppose |f| > c. This is impossible because the individual flows through the edges between partition s to partition t cannot be greater than the respective capacities of those edges. The total flow through the edges between partition s to partition t cannot be greater than the total capacities of those edges combined: |f| ≤ c.
* Suppose |f| < c. This is impossible because the edges between partition s to partition t consist of the saturated edges from G_f that we had removed. Since the edges are saturated, the total flow through these edge is equal to the sum of the capacities of the edges. The total flow in the graph can be no less than the capacities of the cut: |f| ≥ c.
* Thus, |f|= c.

If the residual graph G_f contains no augmenting path, |f| is equal to the capacity of some cut c of G.

**3. If |f| is equal to the capacity of some cut c of G, then f is a maxflow.**

* We know that for all valid flows f, |f|≤ min(c_1, c_2, ..., c_k), where c_i is some cut of G_f.
* Then, if |f| = c, it follows that |f| is a maxflow.
* Suppose not. Then, there exists an f' such that |f'|> |f|. However, we would then have |f'| > c, but c = min(c_1, c_2, ..., c_k), so |f'|> c, then |f'| > min(c_1, c_2, ..., c_k), which breaks the initial rule.

If |f| is equal to the capacity of some cut c of G, then f is a maxflow.

**Time complexity:** O(|f|(V + E)), or O(C (V+E)), where C is the sum of the capacities of the edges directed out of s.

### Assignment of Cell Phones to Towers

**Problem description:** We are given a set of cell phones and a set of cell towers. We also know the distance between any cell phone and any cell tower. We want to pair each cell phone to exactly one tower, provided that the distnace between the two is within r. Each cell phone tower t can be paired with at most c_t phones. Provide an algorithm that finds the maximum number of pairings given these constraints.

**Algorithm:**

```pseudocode
create source node s and sink node u

foreach tower t,
	create node n_t
	add edge (n_t, u) with capacity c_t

foreach cell phone p,
	create node n_p
	foreach tower t,
		if D(p, t) <= r,
			add edge (n_p, n_t) with capacity 1
	add edge (s, n_p) with capacity 1

run Ford-Fulkerson on created network
return |f_max|
```

**Proof of correctness:** The capacity 1 on every edge (s, n_p) ensures that each cell phone is paired with no more than one tower. The capacity c_t on every edge (n_t, u) ensures that each tower can be paired with at most c_t cell phones. We also add edges between n_p and n_t only if their distances are within r.

**Time complexity:** O(n(n+m)), where n is the number of cell phones, and m is the number of towers.

## NP-Completeness

1. Suppose Y is polynomial time reducible to X. If X is solveable in PT, then Y is solveable in PT.
   * By definition, the input and output transformations take PT.
   * It is given that X is solveable in PT.
   * Then, since the runtime of Y is given by the input/output transformations and the solving of X, Y is also PT.
2. Suppose Y is polynomial time reducible to X. If Y is not solveable in PT, then X is not solveable in PT.
   * Suppose that X is solveable in PT.
   * Then, this basically reduces to case 1, which leads to the conclusion that Y is solveable in PT.
   * This contradicts the supposition that Y is not solveable in PT, so X is not solveable in PT.

### Max Independence Set

**Problem description:** An independence set is defined as a subset of nodes of an undirected graph G where no pair of nodes are adjacent (there is no edge between any pair of nodes in the set). The goal is to find the independence set of maximum size of some graph G. Show that the max clique problem is polynomial-time reducible to the max independence set problem.

**Proof:** First of all, we will prove that an independence set on the complement graph G' is a clique on G and vice versa.

* Suppose we have some independence set S on G'. By definition, for every pair of nodes x and y in S, x and y are not connected by an edge.
* Then, by definition of a complement of a graph, x and y are connected by an edge in G.
* Thus, every pair of nodes in S is connected by an edge in G. S is therefore a clique on G.
* For each pair of nodes x and y in S, x and y are connected by an edge in G.
* By definition of a complement of a graph, x and y are not connected by an edge in G'.
* Thus, every pair of nodes in S is not connected by an edge in G', so S is an independence set on G.

Next, we will prove that the max independence set on G' is equal to the max clique on G.

* Suppose S is the max independence set on G'.
* By way of contradiction, suppose S is not the max clique on G.
* Since S is not the max clique on G, there exists a clique of greater size S'.
* Since a clique on G is an independence set on G', then S' is an independence set on G'.
* Since |S'| > |S|, this contradicts the supposition that S is the max independence set on G'.

Thus, the transformation between the two problems is as follows:

1. Take the input graph G for the max clique problem, find its complement G', and input that graph to the max independence set problem. -- O(n^2)
2. Solve the max independence set problem.
3. Send the output of the max independence problem as the solution for the max clique problem. -- O(1)

Since the input and output transformations can be done in polynomial time, the max clique problem is polynomial time reducible to the max independence set problem.

### Min Vertex Cover

**Problem description:** We are given a connected undirected graph G. A vertex cover is defined as a subset of nodes in G such that every edge in G has an endpoint in the vertex cover. The goal is to find the vertex cover on G of minimum size. Show that the min vertex cover problem is polynomial time reducible to the max independence set problem.

**Proof:** We will show that if S is the max independence set on G, then V - S is the min vertex cover on G.

* Suppose we take any pair of nodes x and y in S. By definition of an independence set, there are no edges between x and y.
* Let S' = V - S, where V is the set of all nodes in G. Then, since there are no edges between any pairs of edges in S, it follows that all edges in G must have endpoints in S'.
* Thus, S' is a vertex cover by definition.
* S' must also be the min vertex cover. Suppose that it isn't. Then, there exists at least one node n that we can remove S' such that S' - n is still a vertex cover.
* By definition of the max independence set, there exists an edge between n and m, where m is a node in the max independence set S. Since neither n nor m are in S' - n, the edge (n, m) is not covered by S' - n.
* This contradicts the supposition that S' - n is still a vertex cover. Thus, S' must be the min vertex cover.

Thus, the transformation between the two problems is as follows:

1. Keep the input of the min vertex problem and the input of the max independence set the same. -- O(1) time
2. Solve the max independence problem.
3. Take the output of the max independence set problem S, and use V - S as the output of the min vertex problem. -- O(n)

Since the input and output transformations can be done in polynomial time, the min vertex cover problem is polynomial time reducible to the max independence problem.

### Min Set Cover

**Problem description:** We are given a set of sets. A set cover is defined as a subset of the sets such that every element is contained in the union of this subset. In other words, the union of this subset equals the union of the entire set of sets. The goal is to find the set cover of minimum size. Show that vertex cover is polynomial time reducible to set cover.

**Proof:**

* We are given a graph G as the input to the vertex cover.
* For each node in G, let us create a set comprising of the edges connected to the node.
* Then, let us run set cover on the set of all sets of edges.
* Set cover will return the minimum size subset of sets such that all edges are contained within the union of the sets.
* Since sets of edges correspond to the nodes they are connected to, this by definition is the min vertex cover.

Thus, we require an O(e) input transformation, and no output transformation, so vertex cover is polynomial time reducible to set cover.
