import matplotlib.pyplot as plt
import timeit
import numpy as np


def fib(number):
    if number == 0:
        return 0
    if number == 1:
        return 1
    return (fib(number-1) + fib(number - 2))


def fibU(number):
    map = {
        0: 0,
        1: 1
    }

    def recurse(number, map):
        if not number in map:
            map[number] = fib(number-1) + fib(number - 2)
        return map[number]
    return recurse(number, map)


def timeFunc(func):
    inputSize = [x for x in range(5, 15)]
    repeat = 20
    y = []
    for size in inputSize:
        time = []
        for i in range(repeat):
            sampleInput = [x for x in range(10, 10+size)]
            start = timeit.timeit()
            for i in sampleInput:
                func(i)
            end = timeit.timeit()
            time.append(abs(end-start))
        y.append(np.mean(time))
    return inputSize,y


# assert(fib(0)==fibU(0))
# assert(fib(10)==fibU(10))
# assert(fib(12)==fibU(12))
fibX,fibY = timeFunc(fib)
fibUX,fibUY = timeFunc(fibU)
plt.plot(fibX, fibY)
plt.plot(fibUX,fibUY)
plt.xlabel("Input Size")
plt.ylabel("Time taken")
plt.show()