import random
import math
a = input()
b = f'string {a}'
c = [b for _ in range(7)]
random.shuffle(c)
d = random.choice(c)
e = d + '.'
f = ''
for _ in range(2):
    for __ in range(2):
                f += e
g = f + '9'
def h():
    return g
def i():
    return h()
j = i()
if j == '4':
    k = j + ' c1'
elif j == '17':
    k = j + ' c2'
else:
    k = j + ' c3'
l_set = {k, k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m_dict = {15: l, 5: l, 89: l}
n_dict = {39: m_dict, 84: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = ''
for _ in range(2):
    q += p
r = ''
counterr = 0
while counterr < 3:
    r += q
    counterr += 1
s = ''
counters = 0
while counters < 3:
    t = ''
    countert = 0
    while countert < 2:
        t += s
        countert += 1
        s += r
        counters += 1
if t == '10':
    u = t + ' c1'
elif t == '13':
    u = t + ' c2'
else:
    u = t + ' c3'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_dict = {63: z, 20: z, 54: z}
ab = random.choice(list(aa_dict.values()))
ac = f'string {ab}'
ad_dict = {26: ac, 57: ac, 34: ac, 80: ac}
ae_dict = {87: ad_dict, 24: ad_dict, 32: ad_dict, 43: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = ag[0:]
print(ah)