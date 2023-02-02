n = int(input())
k = 0
def createVector(n, k, prefix) :
    if (len(prefix) == n):
        print(prefix)
        return
    if (len(prefix) == 0):
        createVector(n, k, '0' + prefix )
        createVector(n, k + 1, '1'+ prefix)
    else :    
        if (k == 0):
            createVector(n, 0, prefix + "0" )
            createVector(n, 1, prefix + "1")
        else:
            createVector(n, 0, prefix + "1")
            createVector(n, 1, prefix + "0")

createVector(n, 0, "")