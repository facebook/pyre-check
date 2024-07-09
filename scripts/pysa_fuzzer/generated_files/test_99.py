import random
import math

a = input()
b_list = [a for _ in range(5)]
c = random.choice(b_list)
d = ''
for _ in range(4):
    for __ in range(5):
                d += c
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
def g():
    return f
def h():
    return g()
i = h()
j_set = {i, i, i, i, i, i, i, i, i}
j = random.choice(list(j_set))
def k():
    return j
l = k()
m = l[0:]
n = ''
for _ in range(6):
        if _ == 3:
            continue
        n += m
o = n[0:]
p = o + '.'
q = ''
for _ in range(2):
    for __ in range(3):
                q += p
r = (q, q, q)
s, t, u = r
v = s + t + u
w_set = {v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = ''
for _ in range(3):
    y = ''
    for _ in range(4):
        z = ''
        for _ in range(2):
            z += y
            y += x
        x += w
aa_dict = {34: z, 60: z, 43: z, 95: z, 93: z, 4: z, 86: z, 45: z}
ab_dict = {16: aa_dict, 62: aa_dict, 97: aa_dict, 2: aa_dict, 42: aa_dict, 25: aa_dict, 93: aa_dict, 47: aa_dict, 47: aa_dict, 99: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae_dict = {10: ad, 37: ad, 46: ad, 71: ad, 69: ad, 59: ad, 88: ad, 51: ad, 70: ad, 63: ad}
af_dict = {98: ae_dict, 52: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = ah[0:]
aj_dict = {53: ai, 18: ai, 27: ai, 36: ai, 46: ai, 21: ai, 19: ai}
ak = random.choice(list(aj_dict.values()))
print(ak)