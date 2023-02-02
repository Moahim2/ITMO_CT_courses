n = int(input())

max = 3 ** (n - 1)
k = 0
def createVector(n, prefix) :
    global k
    global max
    if (len(prefix) == n):
        if (k < max):
            k += 1
            print(prefix)
            prefix2 = ""
            prefix3 = ""
            for i in range(n):
                prefix2 += str((int(prefix[i]) + 1) % 3)
                prefix3 += str((int(prefix[i])  + 2) % 3)
            print(prefix2)
            print(prefix3)
            return
        return    
    createVector(n, prefix + '0')
    createVector(n, prefix + '1')
    createVector(n, prefix + "2")  
createVector(n, "") 