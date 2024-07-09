import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '19':
    d = b + 'c2'
else:
    d = c + 'c3'
e = f'string {d}'
f = ''
for _ in range(7):
        if _ == 4:
            continue
        f += e
g = ''
counterg = 0
while counterg < 2:
    h = ''
    counterh = 0
    while counterh < 2:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = ''
for _ in range(3):
    j = ''
    for _ in range(3):
        k = ''
        for _ in range(2):
            k += j
            j += i
        i += h
l = f'string {k}'
m = ''
counterm = 0
while counterm < 2:
    n = ''
    countern = 0
    while countern < 5:
        n += m
        countern += 1
        m += l
        counterm += 1
o = n + '.'
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 2:
        q += p
        counterq += 1
        p += o
        counterp += 1
r_list = [q for _ in range(9)]
s_list = [r_list for _ in range(10)]
t_list = [s_list for _ in range(7)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
def x():
    return w
def y():
    return x()
z = y()
aa = f'string {z}'
ab = [aa for _ in range(6)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ac[0:]
ae = f'string {ad}'
af_dict = {94: ae, 43: ae, 68: ae, 7: ae}
ag_dict = {42: af_dict, 80: af_dict, 90: af_dict, 63: af_dict, 4: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj_dict = {9: ai, 60: ai}
ak_dict = {59: aj_dict, 83: aj_dict, 10: aj_dict, 62: aj_dict, 14: aj_dict, 98: aj_dict}
al_dict = {83: ak_dict, 31: ak_dict, 55: ak_dict, 56: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
ao = random.choice(list(an.values()))
ap = ao + '7'
aq = ap + '7'
ar = aq + '8'
print(ar)