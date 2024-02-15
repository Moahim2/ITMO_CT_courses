prefix = []
def createVector(n, prefix):
    if (len(prefix) == n):
        print(*prefix, sep=" ")
        return
    for i in range(1, n + 1, 1):
        prefix1 = prefix
        if not(i in prefix1):
            prefix1.append(i)
            createVector(n, prefix1)
createVector(int(input()), prefix)