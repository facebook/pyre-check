import random
import math

a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = (b, b, b)
d, e, f = c
g = d + e + f
h = [g for _ in range(6)]
random.shuffle(h)
i = random.choice(h)
j_set = {i, i, i, i, i, i, i, i, i}
j = random.choice(list(j_set))
k_list = [j for _ in range(8)]
l = random.choice(k_list)
m_dict = {95: l, 92: l, 77: l, 40: l, 76: l, 4: l}
n = random.choice(list(m_dict.values()))
o = ''
for _ in range(2):
    for __ in range(4):
                o += n
p = ''
counterp = 0
while counterp < 3:
    q = ''
    counterq = 0
    while counterq < 3:
        r = ''
        counterr = 0
        while counterr < 4:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = [r for _ in range(8)]
random.shuffle(s)
t = random.choice(s)
u_list = [t for _ in range(5)]
v_list = [u_list for _ in range(10)]
w = random.choice(v_list)
x = random.choice(w)
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ''
for _ in range(5):
    ad += ac
if ad == ad:
    ag = ad + 'c1'
elif ad == '17':
    ag = ae + 'c2'
else:
    ag = af + 'c3'
ah = ''
for _ in range(2):
    ai = ''
    for _ in range(5):
        ai += ah
        ah += ag
aj = ''
counteraj = 0
while counteraj < 3:
    ak = ''
    counterak = 0
    while counterak < 2:
        al = ''
        counteral = 0
        while counteral < 4:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
am_list = [al for _ in range(2)]
an = random.choice(am_list)
ao_set = {an, an, an, an}
ao = random.choice(list(ao_set))
ap = f'string {ao}'
print(ap)