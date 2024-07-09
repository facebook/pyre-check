import random
import math

a = input()
b_dict = {20: a, 87: a, 53: a, 37: a, 99: a}
c_dict = {52: b_dict, 32: b_dict, 58: b_dict, 96: b_dict, 2: b_dict, 36: b_dict, 15: b_dict, 90: b_dict, 21: b_dict}
d_dict = {54: c_dict, 6: c_dict, 76: c_dict, 56: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h_dict = {72: g, 64: g, 11: g, 26: g, 35: g, 44: g, 61: g, 65: g}
i_dict = {55: h_dict, 10: h_dict, 59: h_dict, 20: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = ''
for _ in range(5):
    m = ''
    for _ in range(5):
        m += l
        l += k
if m == m:
    p = m + 'c1'
elif m == '16':
    p = n + 'c2'
else:
    p = o + 'c3'
q = f'string {p}'
r = (q, q, q)
s, t, u = r
v = s + t + u
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = [aa for _ in range(8)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ac + '6'
ae = ''
counterae = 0
while counterae < 4:
    af = ''
    counteraf = 0
    while counteraf < 3:
        af += ae
        counteraf += 1
        ae += ad
        counterae += 1
ag_set = {af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = ''
counterah = 0
while counterah < 5:
    ah += ag
    counterah += 1
ai = ''
for _ in range(5):
    aj = ''
    for _ in range(5):
        ak = ''
        for _ in range(2):
            ak += aj
            aj += ai
        ai += ah
al = ak + '.'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = aq + '1'
at = ar + '1'
au = at + '6'
av_dict = {18: au, 31: au, 36: au}
aw = random.choice(list(av_dict.values()))
ax_list = [aw for _ in range(10)]
ay = random.choice(ax_list)
print(ay)