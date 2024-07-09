import random
import math

a = input()
b = a[0:]
c = [b for _ in range(6)]
random.shuffle(c)
d = random.choice(c)
e = d + '.'
f = e + '4'
g = f + '6'
h = g + '1'
i = h[0:]
j = f'string {i}'
k = ''
for _ in range(8):
        if _ == 2:
            continue
        k += j
l = ''
for _ in range(5):
        if _ == 5:
            continue
        l += k
m = (l, l, l)
n, o, p = m
q = n + o + p
r = ''
for _ in range(2):
    for __ in range(2):
                r += q
s_list = [r for _ in range(10)]
t = random.choice(s_list)
u = ''
counteru = 0
while counteru < 5:
    v = ''
    counterv = 0
    while counterv < 3:
        w = ''
        counterw = 0
        while counterw < 3:
            w += v
            counterw += 1
            v += u
            counterv += 1
        u += t
        counteru += 1
x = [w for _ in range(7)]
random.shuffle(x)
y = random.choice(x)
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = [ad for _ in range(10)]
random.shuffle(ae)
af = random.choice(ae)
ag = af[0:]
ah_dict = {35: ag, 53: ag, 90: ag, 72: ag, 68: ag}
ai_dict = {67: ah_dict, 46: ah_dict, 93: ah_dict, 18: ah_dict, 19: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = ''
for _ in range(3):
    for __ in range(5):
                al += ak
print(al)