input = [2, 3, 4, 5, 6]
target = 12


def simple(input, target):
    for x in range(len(input)):
        remain = target - input[x]
        for y in range(len(input)):
            if input[y] == remain and (x!=y):
                return ([x, y])
    return [-1, -1]


def upgrade(input, target):
    tmp = {}
    for i, num in enumerate(input):
        if target - num in tmp:
            return [tmp[target-num], i]
        tmp[num] = i
    return [-1, -1]


#assert(upgrade(input, target) == simple(input, target))

print(simple(input, target))
