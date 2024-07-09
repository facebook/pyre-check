import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '19':
    d = b + 'c2'
else:
    d = c + 'c3'
e = d + '5'
f = e + '3'
g = f + '7'
def h():
    return g
i = h()
j = [i for _ in range(8)]
random.shuffle(j)
k = random.choice(j)
l = ''
for _ in range(9):
        if _ == 4:
            continue
        l += k
m = ''
counterm = 0
while counterm < 3:
    m += l
    counterm += 1
n = f'string {m}'
o_set = {n, n, n, n, n}
o = random.choice(list(o_set))
p = (o, o, o)
q, r, s = p
t = q + r + s
u = ''
for _ in range(5):
        if _ == 3:
            continue
        u += t
v = u + '7'
w = v + '7'
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(5):
        if _ == 2:
            break
        z += y
aa = ''
for _ in range(7):
        if _ == 3:
            break
        aa += z
ab = ''
for _ in range(6):
        if _ == 4:
            continue
        ab += aa
ac = ab + '.'
ad = ''
for _ in range(5):
    ae = ''
    for _ in range(4):
        af = ''
        for _ in range(5):
            af += ae
            ae += ad
        ad += ac
ag_dict = {59: af, 48: af, 53: af, 37: af, 19: af, 81: af, 4: af, 90: af}
ah_dict = {100: ag_dict, 74: ag_dict, 93: ag_dict, 4: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
print(aj)