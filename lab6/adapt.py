L = 200000
def create(n):
    with open(f"fm{n}", 'w') as f:
        for _ in range(L):
            f.write("1\n")


for i in range(1, 21):
    create(i)

