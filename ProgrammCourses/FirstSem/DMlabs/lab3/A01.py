
def createVector(n, prefix) :
    if (len(prefix) == n):
        print(prefix)
        return
    createVector(n, prefix + '0')
    createVector(n, prefix + '1')    
createVector(int(input()), "")