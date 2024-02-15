a = input()
lenS = len(a)


countZeroes = 0
countUnit = 0
i = 1  
while (i != lenS + 1 and a[-i] != '1'):
    countUnit += 1
    i += 1
if (i != lenS + 1):
    print(a[0 : lenS - countUnit - 1] + "0" + '1' * countUnit)
else:
    print('-')
i = 1    
while (i != lenS + 1 and a[-i] != '0'):
    countZeroes += 1
    i += 1
if (i != lenS + 1):
    print(a[0 : lenS - countZeroes - 1] + "1" + '0' * countZeroes)
else:
    print('-')

    