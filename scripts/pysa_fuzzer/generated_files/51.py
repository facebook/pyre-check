import random
import math
a = input()
b = f'string {a}'
def c():
    return b
def d():
    return c()
e = d()
f_set = {e, e, e}
f = random.choice(list(f_set))
g = ''
counterg = 0
while counterg < 4:
    g += f
    counterg += 1
h = g[0:]
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
def m():
    return l
def n():
    return m()
o = n()
p = (o, o, o)
q, r, s = p
t = q + r + s
u = f'string {t}'
v = ''
for _ in range(3):
    v += u
w = ''
for _ in range(3):
    x = ''
    for _ in range(5):
        x += w
        w += v
y_list = [x for _ in range(3)]
z_list = [y_list for _ in range(5)]
aa_list = [z_list for _ in range(3)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
def ae():
    return ad
af = ae()
ag_dict = {28: af, 34: af, 71: af}
ah = random.choice(list(ag_dict.values()))
ai_list = [ah for _ in range(2)]
aj_list = [ai_list for _ in range(9)]
ak_list = [aj_list for _ in range(10)]
al = random.choice(ak_list)
am = random.choice(al)
an = random.choice(am)
ao = f'string {an}'
ap_dict = {9: ao, 64: ao, 83: ao}
aq_dict = {9: ap_dict, 68: ap_dict, 9: ap_dict, 96: ap_dict, 89: ap_dict}
ar = random.choice(list(aq_dict.values()))
at = random.choice(list(ar.values()))
print(at)