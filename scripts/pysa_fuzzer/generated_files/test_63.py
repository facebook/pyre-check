import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    b += a
    counterb += 1
c = ''
for _ in range(5):
    for __ in range(4):
                c += b
def d():
    return c
e = d()
f_set = {e, e, e, e, e}
f = random.choice(list(f_set))
def g():
    return f
def h():
    return g()
def i():
    return h()
j = i()
k = ''
for _ in range(5):
    l = ''
    for _ in range(2):
        l += k
        k += j
m = ''
for _ in range(5):
    n = ''
    for _ in range(4):
        n += m
        m += l
o = n + '.'
p = o + '.'
q = [p for _ in range(7)]
random.shuffle(q)
r = random.choice(q)
s = r[0:]
t = ''
for _ in range(3):
    u = ''
    for _ in range(5):
        u += t
        t += s
if u == u:
    x = u + 'c1'
elif u == '12':
    x = v + 'c2'
else:
    x = w + 'c3'
y = x + '.'
z_list = [y for _ in range(8)]
aa_list = [z_list for _ in range(3)]
ab_list = [aa_list for _ in range(2)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
af_dict = {25: ae, 54: ae, 17: ae, 81: ae, 77: ae, 49: ae}
ag = random.choice(list(af_dict.values()))
ah = ag + '4'
ai = ah + '9'
aj = ''
counteraj = 0
while counteraj < 2:
    ak = ''
    counterak = 0
    while counterak < 3:
        al = ''
        counteral = 0
        while counteral < 4:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
print(al)