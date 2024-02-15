n = int(input())
str = input()
arr = []
arr = str.split(' ')
alh = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
       'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
t = ""
answer = ""
j = 0
for i in range(0, n):
    #print(answer)
    k = int(arr[i])
    if (k < len(alh)):
        answer += alh[k]
    else:
        if (t == ""):
            t = answer[0]
        while(k >= len(alh) and j <= len(answer) - 2):
            if (not(t in alh)):
                alh.append(t)
                t = t[-1]
                continue
            j += 1
            t += answer[j]
        if (not(t in alh)):
                alh.append(t)
                t = t[-1]
        if (k >= len(alh)):
            t = alh[int(arr[i - 1])] + alh[int(arr[i - 1])][0]
            alh.append(t)
            answer += alh[-1]
            j = len(answer) - len(alh[-1])
            t = answer[j]
        else :
            answer += alh[k]
print(answer)
#print(alh)



            
            
            
            
