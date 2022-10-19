A = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]
B = [1,3,5,7,9]

def findMinRec(arr,i,sumCalculate,sumTotal):
    if (i==len(arr)-1):
        return abs((sumTotal-sumCalculate)-sumCalculate)
    return min(findMinRec(arr,i+1,sumCalculate+arr[i+1],sumTotal),
               findMinRec(arr,i+1,sumCalculate,sumTotal))

def func(A):
    total = sum(A)
    return findMinRec(A,0,0,total)
        
        

print(func(B))
