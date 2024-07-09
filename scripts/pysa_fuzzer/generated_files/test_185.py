import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '13':
    d = b + 'c2'
else:
    d = c + 'c3'
e_dict = {1: d, 96: d, 83: d, 89: d, 9: d, 38: d, 17: d, 12: d, 58: d, 83: d}
f_dict = {19: e_dict, 64: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = h + '4'
j = i + '5'
k = j + '9'
l = ''
counterl = 0
while counterl < 4:
    m = ''
    counterm = 0
    while counterm < 5:
        n = ''
        countern = 0
        while countern < 5:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o_dict = {18: n, 27: n, 78: n, 23: n, 78: n, 78: n, 81: n}
p = random.choice(list(o_dict.values()))
q_set = {p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = f'string {q}'
def s():
    return r
t = s()
u = [t for _ in range(9)]
random.shuffle(u)
v = random.choice(u)
w = ''
for _ in range(5):
        if _ == 3:
            continue
        w += v
x = w + '.'
y = ''
countery = 0
while countery < 3:
    z = ''
    counterz = 0
    while counterz < 5:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = ''
for _ in range(2):
    aa += z
ab = f'string {aa}'
ac_list = [ab for _ in range(8)]
ad_list = [ac_list for _ in range(9)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = f'string {ak}'
if al == al:
    ao = al + 'c1'
elif al == '16':
    ao = am + 'c2'
else:
    ao = an + 'c3'
print(ao)