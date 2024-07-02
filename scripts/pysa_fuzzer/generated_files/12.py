import random
import math
a = input()
def b():
    return a
c = b()
d = [c for _ in range(9)]
random.shuffle(d)
e = random.choice(d)
f = f'string {e}'
g_dict = {97: f, 62: f, 68: f}
h = random.choice(list(g_dict.values()))
i = h[0:]
j_set = {i, i, i, i, i}
j = random.choice(list(j_set))
k = ''
for _ in range(3):
    k += j
l = ''
counterl = 0
while counterl < 5:
    m = ''
    counterm = 0
    while counterm < 4:
        m += l
        counterm += 1
        l += k
        counterl += 1
n = ''
for _ in range(3):
    n += m
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(9):
        if _ == 1:
            break
        q += p
r = ''
for _ in range(7):
        if _ == 3:
            continue
        r += q
s = f'string {r}'
t = s + '1'
u = t + '6'
v = u + '.'
w = f'string {v}'
def x():
    return w
def y():
    return x()
z = y()
aa_dict = {98: z, 81: z, 9: z, 26: z, 96: z, 1: z, 76: z, 41: z, 13: z, 6: z}
ab_dict = {77: aa_dict, 32: aa_dict, 69: aa_dict, 21: aa_dict}
ac_dict = {73: ab_dict, 24: ab_dict, 80: ab_dict, 37: ab_dict, 70: ab_dict, 24: ab_dict, 1: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
print(af)