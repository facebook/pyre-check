import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = [b for _ in range(8)]
random.shuffle(c)
d = random.choice(c)
e = [d for _ in range(10)]
random.shuffle(e)
f = random.choice(e)
g = ''
for _ in range(4):
    for __ in range(2):
                g += f
h = ''
for _ in range(3):
    for __ in range(2):
                h += g
def i():
    return h
def j():
    return i()
k = j()
l = [k for _ in range(10)]
random.shuffle(l)
m = random.choice(l)
n_list = [m for _ in range(5)]
o = random.choice(n_list)
p = o[0:]
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u[0:]
w = ''
for _ in range(4):
    x = ''
    for _ in range(3):
        y = ''
        for _ in range(5):
            y += x
            x += w
        w += v
z = [y for _ in range(10)]
random.shuffle(z)
aa = random.choice(z)
ab_list = [aa for _ in range(10)]
ac = random.choice(ab_list)
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai_dict = {68: ah, 53: ah, 74: ah, 84: ah, 80: ah, 86: ah, 8: ah}
aj_dict = {89: ai_dict, 31: ai_dict, 53: ai_dict, 4: ai_dict}
ak_dict = {54: aj_dict, 24: aj_dict, 84: aj_dict, 91: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
an = random.choice(list(am.values()))
ao_set = {an, an, an, an}
ao = random.choice(list(ao_set))
ap = ao + '1'
aq = ap + '5'
ar = aq + '7'
print(ar)