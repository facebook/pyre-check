import random
import math

a = input()
b = a + '.'
c = [b for _ in range(9)]
random.shuffle(c)
d = random.choice(c)
e = d + '5'
f = e + '5'
g_dict = {88: f, 45: f, 19: f, 90: f, 77: f}
h_dict = {85: g_dict, 29: g_dict, 6: g_dict, 21: g_dict, 48: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m_set = {l, l}
m = random.choice(list(m_set))
n = f'string {m}'
o = (n, n, n)
p, q, r = o
s = p + q + r
t = [s for _ in range(7)]
random.shuffle(t)
u = random.choice(t)
v = f'string {u}'
w = ''
for _ in range(2):
    for __ in range(3):
                w += v
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = [y for _ in range(7)]
random.shuffle(z)
aa = random.choice(z)
ab = ''
counterab = 0
while counterab < 3:
    ac = ''
    counterac = 0
    while counterac < 5:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
ad = [ac for _ in range(5)]
random.shuffle(ad)
ae = random.choice(ad)
af_set = {ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = af[0:]
ah = ''
for _ in range(5):
    ai = ''
    for _ in range(4):
        aj = ''
        for _ in range(2):
            aj += ai
            ai += ah
        ah += ag
print(aj)