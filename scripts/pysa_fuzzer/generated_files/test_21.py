import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(5):
                b += a
c = b[0:]
d_set = {c, c, c, c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e = ''
countere = 0
while countere < 4:
    f = ''
    counterf = 0
    while counterf < 5:
        g = ''
        counterg = 0
        while counterg < 3:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = k[0:]
m = ''
counterm = 0
while counterm < 2:
    n = ''
    countern = 0
    while countern < 4:
        n += m
        countern += 1
        m += l
        counterm += 1
o = n[0:]
p_set = {o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = [p for _ in range(7)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(8):
        if _ == 1:
            break
        s += r
t = (s, s, s)
u, v, w = t
x = u + v + w
y_list = [x for _ in range(4)]
z = random.choice(y_list)
aa_list = [z for _ in range(5)]
ab_list = [aa_list for _ in range(8)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = f'string {ad}'
af_list = [ae for _ in range(7)]
ag = random.choice(af_list)
ah = ag[0:]
ai = ''
for _ in range(5):
    for __ in range(5):
                ai += ah
print(ai)