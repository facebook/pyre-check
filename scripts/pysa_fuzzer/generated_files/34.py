import random
import math
a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f_list = [e for _ in range(8)]
g_list = [f_list for _ in range(8)]
h = random.choice(g_list)
i = random.choice(h)
j = [i for _ in range(7)]
random.shuffle(j)
k = random.choice(j)
l = (k, k, k)
m, n, o = l
p = m + n + o
q_list = [p for _ in range(2)]
r_list = [q_list for _ in range(2)]
s = random.choice(r_list)
t = random.choice(s)
u = ''
counteru = 0
while counteru < 5:
    v = ''
    counterv = 0
    while counterv < 2:
        w = ''
        counterw = 0
        while counterw < 5:
            w += v
            counterw += 1
            v += u
            counterv += 1
        u += t
        counteru += 1
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = f'string {ab}'
if ac == '4':
    ad = ac + ' c1'
elif ac == '13':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ''
for _ in range(2):
    for __ in range(3):
                ae += ad
af_dict = {13: ae, 78: ae, 67: ae, 68: ae}
ag = random.choice(list(af_dict.values()))
ah = ''
counterah = 0
while counterah < 3:
    ai = ''
    counterai = 0
    while counterai < 2:
        ai += ah
        counterai += 1
        ah += ag
        counterah += 1
aj_dict = {86: ai, 44: ai, 96: ai, 89: ai, 70: ai}
ak_dict = {10: aj_dict, 56: aj_dict}
al_dict = {39: ak_dict, 83: ak_dict, 23: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
ao = random.choice(list(an.values()))
ap = ao + '2'
aq = ap + '7'
ar = aq + '9'
at_set = {ar, ar, ar, ar, ar, ar, ar}
at = random.choice(list(at_set))
au = f'string {at}'
av = [au for _ in range(5)]
random.shuffle(av)
aw = random.choice(av)
ax = f'string {aw}'
print(ax)