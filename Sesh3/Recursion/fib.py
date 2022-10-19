def fib(number):
    if number == 0 :
        return 0
    if number == 1 :
        return 1
    return (fib(number-1) + fib(number - 2))

print(fib(0))
print(fib(1))
print(fib(2))
print(fib(3))
print(fib(14))