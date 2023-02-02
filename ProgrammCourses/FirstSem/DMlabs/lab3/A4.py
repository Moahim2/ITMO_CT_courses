n = int(input())

answers = []
answers.append("0" * n)
print(answers[0])
last = answers[0]
k = 1
y = ""
for k in range(2 ** n - 1):
    y = (last[1 : n])
    last = y + "1"
    if (last in answers) :
        answers.remove(last)
        last = y + "0"
        print(last)
    else:
        print(last)
        answers.append(last)