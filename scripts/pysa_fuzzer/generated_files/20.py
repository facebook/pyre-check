import random
import math
a = input()
b = a + '.'
c = ''
for _ in range(4):
    d = ''
    for _ in range(2):
        d += c
        c += b
e = ''
for _ in range(4):
    e += d
f_dict = {36: e, 30: e, 2: e, 39: e, 48: e, 49: e}
g = random.choice(list(f_dict.values()))
h = f'string {g}'
i_list = [h for _ in range(7)]
j_list = [i_list for _ in range(5)]
k = random.choice(j_list)
l = random.choice(k)
m = l[0:]
n = ''
for _ in range(8):
        if _ == 4:
            break
        n += m
o_dict = {97: n, 81: n, 79: n, 5: n, 12: n, 18: n, 64: n}
p = random.choice(list(o_dict.values()))
q_list = [p for _ in range(7)]
r = random.choice(q_list)
s_set = {r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(10):
        if _ == 1:
            break
        t += s
u = (t, t, t)
v, w, x = u
y = v + w + x
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
if ad == '7':
    ae = ad + ' c1'
elif ad == '15':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ae + '.'
ag = f'string {af}'
ah = ''
for _ in range(4):
    for __ in range(5):
                ah += ag
print(ah)