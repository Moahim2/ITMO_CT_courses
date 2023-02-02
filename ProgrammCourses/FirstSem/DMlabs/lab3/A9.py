
def createVector(n, balance, prefix) :
    if (len(prefix) == 2 * n):
        print(prefix)
        return
    if (balance + 1 < 2 * n - len(prefix)): 
        createVector(n, balance + 1, prefix + '(')    
    if (balance > 0):         
        createVector(n, balance - 1, prefix + ')')
createVector(int(input()), 0, "")