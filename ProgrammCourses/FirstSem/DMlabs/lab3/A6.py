vectors = []
k = 0
def createVector(n, prefix) :
    global vectors
    global k
    if (len(prefix) == n):
        k += 1
        vectors.append(prefix)
        return
    prefix1 = prefix + '0'
    prefix2 = prefix + '1'  
    createVector(n, prefix1)
    if (len(prefix) == 0 or prefix[-1] == '0'):
        createVector(n, prefix2)
createVector(int(input()), "")

print(k)
for i in range(k):
    print(vectors[i])