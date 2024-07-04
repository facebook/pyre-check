import random
import math
a = input()
b = a[0:]
c = ''
counterc = 0
while counterc < 5:
    d = ''
    counterd = 0
    while counterd < 2:
        e = ''
        countere = 0
        while countere < 2:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f = e + '.'
g_list = [f for _ in range(7)]
h = random.choice(g_list)
i_set = {h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(10):
        if _ == 3:
            break
        j += i
k = j[0:]
l = ''
for _ in range(5):
    for __ in range(2):
                l += k
m = (l, l, l)
n, o, p = m
q = n + o + p
r = q + '8'
s = r + '3'
t = s + '6'
u = ''
for _ in range(6):
        if _ == 2:
            continue
        u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_dict = {14: z, 60: z, 16: z, 68: z, 42: z, 59: z, 42: z, 11: z}
ab_dict = {45: aa_dict, 59: aa_dict, 70: aa_dict, 75: aa_dict, 44: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = ''
for _ in range(10):
        if _ == 4:
            break
        ae += ad
af = [ae for _ in range(9)]
random.shuffle(af)
ag = random.choice(af)
ah = ag + '.'
ai = ah + '5'
aj = ai + '9'
ak = aj + '2'
al = ak[0:]
print(al)