import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g_list = [f for _ in range(5)]
h = random.choice(g_list)
i = ''
for _ in range(3):
    for __ in range(4):
                i += h
j_dict = {26: i, 98: i, 96: i, 97: i, 82: i, 79: i, 9: i}
k_dict = {18: j_dict, 50: j_dict, 64: j_dict, 67: j_dict, 39: j_dict, 66: j_dict, 98: j_dict, 54: j_dict, 66: j_dict}
l_dict = {15: k_dict, 32: k_dict, 12: k_dict, 49: k_dict, 47: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = random.choice(list(n.values()))
p = ''
for _ in range(3):
    q = ''
    for _ in range(3):
        r = ''
        for _ in range(3):
            r += q
            q += p
        p += o
if r == r:
    u = r + 'c1'
elif r == '12':
    u = s + 'c2'
else:
    u = t + 'c3'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = f'string {z}'
ab = ''
for _ in range(7):
        if _ == 4:
            continue
        ab += aa
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah_dict = {71: ag, 35: ag, 47: ag, 98: ag, 5: ag, 6: ag}
ai_dict = {49: ah_dict, 93: ah_dict, 31: ah_dict, 13: ah_dict}
aj_dict = {95: ai_dict, 85: ai_dict, 33: ai_dict, 99: ai_dict, 55: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an = am[0:]
ao = [an for _ in range(9)]
random.shuffle(ao)
ap = random.choice(ao)
aq = ap + '3'
ar = aq + '5'
at = ar + '.'
au = at + '3'
if au == au:
    ax = au + 'c1'
elif au == '18':
    ax = av + 'c2'
else:
    ax = aw + 'c3'
print(ax)