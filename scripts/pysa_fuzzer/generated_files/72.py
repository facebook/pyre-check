import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f[0:]
h = f'string {g}'
i = ''
for _ in range(9):
        if _ == 4:
            break
        i += h
if i == '1':
    j = i + ' c1'
elif i == '19':
    j = i + ' c2'
else:
    j = i + ' c3'
def k():
    return j
def l():
    return k()
m = l()
if m == '7':
    n = m + ' c1'
elif m == '19':
    n = m + ' c2'
else:
    n = m + ' c3'
o = n[0:]
p_dict = {84: o, 100: o, 4: o, 9: o, 71: o, 6: o}
q_dict = {23: p_dict, 69: p_dict, 87: p_dict, 7: p_dict, 94: p_dict, 86: p_dict, 44: p_dict, 32: p_dict, 58: p_dict, 43: p_dict}
r_dict = {70: q_dict, 77: q_dict, 28: q_dict, 87: q_dict, 35: q_dict, 92: q_dict, 95: q_dict, 13: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = ''
for _ in range(5):
    for __ in range(3):
                aa += z
if aa == '3':
    ab = aa + ' c1'
elif aa == '16':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac = ''
for _ in range(4):
    for __ in range(4):
                ac += ab
ad = [ac for _ in range(8)]
random.shuffle(ad)
ae = random.choice(ad)
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = ''
for _ in range(5):
        if _ == 4:
            continue
        ak += aj
if ak == '7':
    al = ak + ' c1'
elif ak == '20':
    al = ak + ' c2'
else:
    al = ak + ' c3'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
print(aq)