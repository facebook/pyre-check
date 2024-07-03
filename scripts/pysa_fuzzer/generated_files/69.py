import random
import math
a = input()
b = [a for _ in range(7)]
random.shuffle(b)
c = random.choice(b)
d = c[0:]
e = ''
for _ in range(10):
        if _ == 1:
            break
        e += d
f = e[0:]
g = ''
counterg = 0
while counterg < 5:
    g += f
    counterg += 1
h = f'string {g}'
i = ''
for _ in range(5):
    for __ in range(2):
                i += h
j = f'string {i}'
k_dict = {76: j, 62: j}
l_dict = {9: k_dict, 23: k_dict, 66: k_dict, 97: k_dict, 40: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = ''
countero = 0
while countero < 4:
    p = ''
    counterp = 0
    while counterp < 4:
        p += o
        counterp += 1
        o += n
        countero += 1
q_dict = {63: p, 38: p, 43: p, 5: p, 45: p, 74: p, 87: p, 100: p}
r_dict = {39: q_dict, 23: q_dict, 96: q_dict, 72: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = f'string {t}'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = z[0:]
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag_set = {af, af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = [ag for _ in range(10)]
random.shuffle(ah)
ai = random.choice(ah)
aj_list = [ai for _ in range(5)]
ak = random.choice(aj_list)
print(ak)