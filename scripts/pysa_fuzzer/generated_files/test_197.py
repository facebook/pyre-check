import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(4):
        d = ''
        for _ in range(3):
            d += c
            c += b
        b += a
e = (d, d, d)
f, g, h = e
i = f + g + h
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
l = ''
for _ in range(2):
    l += k
if l == l:
    o = l + 'c1'
elif l == '11':
    o = m + 'c2'
else:
    o = n + 'c3'
p_list = [o for _ in range(2)]
q_list = [p_list for _ in range(5)]
r = random.choice(q_list)
s = random.choice(r)
t_set = {s, s, s, s, s, s}
t = random.choice(list(t_set))
u = [t for _ in range(9)]
random.shuffle(u)
v = random.choice(u)
def w():
    return v
def x():
    return w()
def y():
    return x()
z = y()
aa = [z for _ in range(5)]
random.shuffle(aa)
ab = random.choice(aa)
ac = f'string {ab}'
ad = ac[0:]
ae = ''
for _ in range(4):
    af = ''
    for _ in range(5):
        ag = ''
        for _ in range(3):
            ag += af
            af += ae
        ae += ad
ah_list = [ag for _ in range(7)]
ai_list = [ah_list for _ in range(5)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = ak + '.'
am = ''
for _ in range(4):
    for __ in range(5):
                am += al
an = am + '.'
ao = an + '5'
print(ao)