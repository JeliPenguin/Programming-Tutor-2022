import random

r = 1/2
circleCentreX, circleCentreY = 0.5,0.5
precisions = [1000, 5000, 10000, 50000, 100000, 500000]
estimates = []


def hit(x, y):
    return ((x-circleCentreX)**2 + (y-circleCentreY)**2) <= (r**2)


for precision in (precisions):
    hitC = 0
    for _ in range(precision):
        x = random.random()
        y = random.random()
        if hit(x, y):
            hitC += 1
    estimates.append((hitC/precision) / r**2)


for precision, estimate in zip(precisions, estimates):
    print(f"Precision: {precision} Estimate for Pi: {estimate}")
