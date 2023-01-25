def unique_paths(r, c):
    mem = [[0 for _ in range(c)] for _ in range(r)]

    for row in range(r):
        for col in range(c):
            if row == 0 and col == 0:
                mem[row][col] = 1
            else:
                mem[row][col] = mem[row-1][col] + mem[row][col-1]

    print(mem[-1][-1])


# https://leetcode.com/problems/min-cost-climbing-stairs/solutions/3021926/python-dynamic-programming-4-lines-of-code/?orderBy=hot&languageTags=python
def min_cost_climbing_stairs(cost):
    c1, c2 = cost[0], cost[1]
    for i in range(2, len(cost)):
        c1, c2 = c2, min(c1, c2)+cost[i]
    return min(c1, c2)


unique_paths(4,6)