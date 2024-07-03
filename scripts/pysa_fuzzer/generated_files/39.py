import random
import math
a = input()
b = a + '7'
c = b[0:]
if c == '2':
    d = c + ' c1'
elif c == '13':
    d = c + ' c2'
else:
    d = c + ' c3'
e = [d for _ in range(8)]
random.shuffle(e)
f = random.choice(e)
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
for _ in range(4):
    for __ in range(5):
                l += k
m = [l for _ in range(9)]
random.shuffle(m)
n = random.choice(m)
o_set = {n, n, n, n, n, n, n}
o = random.choice(list(o_set))
p = o + '3'
q = f'string {p}'
r_dict = {84: q, 99: q, 69: q, 37: q, 38: q, 61: q, 13: q, 99: q, 36: q}
s_dict = {66: r_dict, 5: r_dict, 50: r_dict, 41: r_dict, 51: r_dict, 56: r_dict, 9: r_dict, 25: r_dict, 36: r_dict}
t_dict = {89: s_dict, 62: s_dict, 28: s_dict, 57: s_dict, 14: s_dict, 83: s_dict, 70: s_dict, 23: s_dict, 93: s_dict, 97: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = ''
for _ in range(9):
        if _ == 3:
            break
        x += w
y = x + '5'
z = ''
for _ in range(6):
        if _ == 1:
            break
        z += y
aa_dict = {25: z, 46: z, 18: z, 9: z}
ab_dict = {7: aa_dict, 60: aa_dict, 79: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = ''
for _ in range(2):
    for __ in range(3):
                ae += ad
af_dict = {52: ae, 90: ae, 18: ae}
ag_dict = {99: af_dict, 56: af_dict}
ah_dict = {4: ag_dict, 63: ag_dict, 82: ag_dict, 38: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = random.choice(list(aj.values()))
al_list = [ak for _ in range(6)]
am = random.choice(al_list)
print(am)