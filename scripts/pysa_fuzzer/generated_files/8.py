import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = [b for _ in range(8)]
random.shuffle(c)
d = random.choice(c)
e_set = {d, d, d, d, d}
e = random.choice(list(e_set))
f = (e, e, e)
g, h, i = f
j = g + h + i
k_list = [j for _ in range(2)]
l_list = [k_list for _ in range(9)]
m = random.choice(l_list)
n = random.choice(m)
def o():
    return n
def p():
    return o()
def q():
    return p()
r = q()
s_set = {r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(5):
    for __ in range(4):
                t += s
u = ''
for _ in range(3):
    for __ in range(5):
                u += t
if u == '6':
    v = u + ' c1'
elif u == '17':
    v = u + ' c2'
else:
    v = u + ' c3'
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = f'string {aa}'
ac = ''
for _ in range(5):
    ad = ''
    for _ in range(2):
        ae = ''
        for _ in range(3):
            ae += ad
            ad += ac
        ac += ab
af = ae + '.'
ag = f'string {af}'
ah = [ag for _ in range(6)]
random.shuffle(ah)
ai = random.choice(ah)
aj = f'string {ai}'
ak = aj + '9'
print(ak)