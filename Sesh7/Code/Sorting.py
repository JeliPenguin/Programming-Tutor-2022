test = [0, 2, 1, 4, 3]

# https://www.geeksforgeeks.org/quick-sort/


def quicksort(arr, low, high):
    if (low < high):
        # Pivot
        pi = partition(arr, low, high)
        # Divide and conquer
        quicksort(arr, low, pi-1)
        quicksort(arr, pi+1, high)


def swap(arr, i, j):
    arr[i], arr[j] = arr[j], arr[i]


def partition(arr, low, high):
    pivot = arr[high]
    i = low-1
    for j in range(low, high):
        if (arr[j] < pivot):
            i += 1
            swap(arr, i, j)
    swap(arr, i+1, high)
    return (i+1)


# https://www.geeksforgeeks.org/heap-sort/
# To heapify subtree rooted at index i.
# n is size of heap


def heapify(arr, N, i):
    largest = i  # Initialize largest as root
    l = 2 * i + 1     # left = 2*i + 1
    r = 2 * i + 2     # right = 2*i + 2

    # See if left child of root exists and is
    # greater than root
    if l < N and arr[largest] < arr[l]:
        largest = l

    # See if right child of root exists and is
    # greater than root
    if r < N and arr[largest] < arr[r]:
        largest = r

    # Change root, if needed
    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]  # swap

        # Heapify the root.
        heapify(arr, N, largest)

# The main function to sort an array of given size


def heapSort(arr):
    N = len(arr)

    # Build a maxheap.
    for i in range(N//2 - 1, -1, -1):
        heapify(arr, N, i)

    # One by one extract elements
    for i in range(N-1, 0, -1):
        arr[i], arr[0] = arr[0], arr[i]  # swap
        heapify(arr, i, 0)


if __name__ == "__main__":
    qS = test.copy()
    quicksort(qS, 0, len(test)-1)
    print(qS)
    # hS = test.copy()
    # heapSort(hS)
    # print(hS)
