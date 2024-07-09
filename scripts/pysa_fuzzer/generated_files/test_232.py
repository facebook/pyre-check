import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(6):
        if _ == 1:
            break
        d += c
e = d + '.'
f = ''
for _ in range(5):
    f += e
g_dict = {21: f, 31: f, 88: f, 55: f, 63: f, 91: f, 72: f, 100: f, 46: f, 58: f}
h_dict = {4: g_dict, 84: g_dict, 96: g_dict, 56: g_dict, 45: g_dict, 81: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = ''
for _ in range(8):
        if _ == 5:
            break
        k += j
l_set = {k, k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = l + '.'
def n():
    return m
o = n()
p = (o, o, o)
q, r, s = p
t = q + r + s
u = t[0:]
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
if w == w:
    z = w + 'c1'
elif w == '12':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = z[0:]
ab = aa + '5'
ac = ab + '4'
ad = ac + '.'
ae = f'string {ad}'
af_list = [ae for _ in range(6)]
ag_list = [af_list for _ in range(3)]
ah = random.choice(ag_list)
ai = random.choice(ah)
print(ai)