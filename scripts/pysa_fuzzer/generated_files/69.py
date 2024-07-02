import random
import math
a = input()
if a == '8':
    b = a + ' c1'
elif a == '14':
    b = a + ' c2'
else:
    b = a + ' c3'
c = b + '9'
d = c + '6'
e = d + '1'
f_list = [e for _ in range(10)]
g = random.choice(f_list)
h = f'string {g}'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
countern = 0
while countern < 4:
    o = ''
    countero = 0
    while countero < 3:
        o += n
        countero += 1
        n += m
        countern += 1
p = o + '.'
q = ''
counterq = 0
while counterq < 5:
    q += p
    counterq += 1
r = f'string {q}'
s = ''
counters = 0
while counters < 2:
    t = ''
    countert = 0
    while countert < 3:
        t += s
        countert += 1
        s += r
        counters += 1
u = ''
for _ in range(2):
    v = ''
    for _ in range(3):
        v += u
        u += t
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab_dict = {27: aa, 19: aa, 12: aa, 84: aa, 6: aa, 82: aa, 21: aa, 38: aa, 6: aa}
ac = random.choice(list(ab_dict.values()))
ad_set = {ac, ac}
ad = random.choice(list(ad_set))
if ad == '5':
    ae = ad + ' c1'
elif ad == '18':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ae + '.'
def ag():
    return af
def ah():
    return ag()
ai = ah()
aj = ''
for _ in range(4):
    ak = ''
    for _ in range(5):
        al = ''
        for _ in range(2):
            al += ak
            ak += aj
        aj += ai
print(al)