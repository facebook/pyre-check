import random
import math
a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 2:
        d = ''
        counterd = 0
        while counterd < 4:
            e = ''
            countere = 0
            while countere < 5:
                f = ''
                counterf = 0
                while counterf < 4:
                    f += e
                    counterf += 1
                    e += d
                    countere += 1
                d += c
                counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
print(f)