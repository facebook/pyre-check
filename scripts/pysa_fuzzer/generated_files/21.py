import random
import math
a = input()
b = a + '6'
c = b + '7'
d_set = {c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e_dict = {92: d, 29: d, 48: d, 70: d}
f_dict = {70: e_dict, 3: e_dict}
g_dict = {23: f_dict, 50: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = ''
for _ in range(3):
    for __ in range(2):
                k += j
def l():
    return k
def m():
    return l()
n = m()
o = f'string {n}'
p = o[0:]
q_set = {p, p, p, p}
q = random.choice(list(q_set))
r = f'string {q}'
if r == '5':
    s = r + ' c1'
elif r == '18':
    s = r + ' c2'
else:
    s = r + ' c3'
t = ''
for _ in range(9):
        if _ == 4:
            break
        t += s
u = (t, t, t)
v, w, x = u
y = v + w + x
z_set = {y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = z[0:]
ab = aa[0:]
ac = ab + '.'
ad_list = [ac for _ in range(2)]
ae_list = [ad_list for _ in range(5)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = [ag for _ in range(8)]
random.shuffle(ah)
ai = random.choice(ah)
print(ai)