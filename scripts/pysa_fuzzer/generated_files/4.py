import random
import math
a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(2):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
def e():
    return d
def f():
    return e()
g = f()
h = g + '6'
i = ''
for _ in range(5):
    for __ in range(5):
                i += h
j = [i for _ in range(9)]
random.shuffle(j)
k = random.choice(j)
l = ''
for _ in range(5):
    m = ''
    for _ in range(5):
        m += l
        l += k
n_set = {m, m, m, m, m, m}
n = random.choice(list(n_set))
o_list = [n for _ in range(6)]
p_list = [o_list for _ in range(2)]
q_list = [p_list for _ in range(9)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
u = ''
for _ in range(4):
    v = ''
    for _ in range(5):
        v += u
        u += t
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = f'string {aa}'
ac = ab + '1'
ad = ac + '2'
ae = [ad for _ in range(9)]
random.shuffle(ae)
af = random.choice(ae)
ag = af + '.'
ah = ''
for _ in range(4):
    for __ in range(4):
                ah += ag
if ah == '3':
    ai = ah + ' c1'
elif ah == '19':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
aj = ''
counteraj = 0
while counteraj < 2:
    aj += ai
    counteraj += 1
ak_list = [aj for _ in range(3)]
al = random.choice(ak_list)
print(al)