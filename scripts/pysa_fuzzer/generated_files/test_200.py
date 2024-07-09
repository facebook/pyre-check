import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(4):
                b += a
c = ''
for _ in range(5):
    d = ''
    for _ in range(3):
        e = ''
        for _ in range(5):
            e += d
            d += c
        c += b
f = e[0:]
def g():
    return f
def h():
    return g()
i = h()
j = i + '2'
k = j + '7'
l = k + '2'
m_set = {l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(5):
    o = ''
    for _ in range(3):
        o += n
        n += m
p = ''
for _ in range(2):
    q = ''
    for _ in range(2):
        r = ''
        for _ in range(5):
            r += q
            q += p
        p += o
def s():
    return r
t = s()
if t == t:
    w = t + 'c1'
elif t == '16':
    w = u + 'c2'
else:
    w = v + 'c3'
x = f'string {w}'
y_list = [x for _ in range(3)]
z = random.choice(y_list)
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac_dict = {100: ab, 70: ab, 20: ab, 88: ab, 85: ab}
ad_dict = {27: ac_dict, 24: ac_dict, 39: ac_dict, 94: ac_dict, 89: ac_dict}
ae_dict = {33: ad_dict, 42: ad_dict, 82: ad_dict, 17: ad_dict, 93: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
ai_list = [ah for _ in range(4)]
aj = random.choice(ai_list)
ak_set = {aj, aj, aj, aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al = ''
counteral = 0
while counteral < 4:
    am = ''
    counteram = 0
    while counteram < 2:
        am += al
        counteram += 1
        al += ak
        counteral += 1
an_set = {am, am, am, am, am}
an = random.choice(list(an_set))
print(an)