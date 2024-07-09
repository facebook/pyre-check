import random
import math

a = input()
b_set = {a, a, a}
b = random.choice(list(b_set))
c = b[0:]
d = c[0:]
e_dict = {76: d, 33: d, 34: d}
f_dict = {50: e_dict, 42: e_dict, 70: e_dict, 24: e_dict, 91: e_dict, 82: e_dict, 99: e_dict, 54: e_dict, 39: e_dict, 8: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = [h for _ in range(8)]
random.shuffle(i)
j = random.choice(i)
if j == j:
    m = j + 'c1'
elif j == '19':
    m = k + 'c2'
else:
    m = l + 'c3'
n = m[0:]
if n == n:
    q = n + 'c1'
elif n == '16':
    q = o + 'c2'
else:
    q = p + 'c3'
r = [q for _ in range(8)]
random.shuffle(r)
s = random.choice(r)
t = ''
countert = 0
while countert < 2:
    u = ''
    counteru = 0
    while counteru < 3:
        v = ''
        counterv = 0
        while counterv < 3:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
w = v + '.'
x = [w for _ in range(9)]
random.shuffle(x)
y = random.choice(x)
z = y[0:]
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ''
for _ in range(9):
        if _ == 1:
            continue
        af += ae
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = f'string {ak}'
am = f'string {al}'
print(am)