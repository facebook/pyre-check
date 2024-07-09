import random
import math

a = input()
b = a + '.'
c_list = [b for _ in range(9)]
d = random.choice(c_list)
e = (d, d, d)
f, g, h = e
i = f + g + h
j = (i, i, i)
k, l, m = j
n = k + l + m
o_set = {n, n, n, n, n, n, n, n}
o = random.choice(list(o_set))
p_set = {o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = ''
counterq = 0
while counterq < 2:
    q += p
    counterq += 1
r = ''
counterr = 0
while counterr < 2:
    s = ''
    counters = 0
    while counters < 3:
        s += r
        counters += 1
        r += q
        counterr += 1
if s == s:
    v = s + 'c1'
elif s == '11':
    v = t + 'c2'
else:
    v = u + 'c3'
w = ''
for _ in range(2):
    x = ''
    for _ in range(4):
        y = ''
        for _ in range(5):
            y += x
            x += w
        w += v
z = y + '.'
aa = z[0:]
ab = [aa for _ in range(10)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ac + '.'
def ae():
    return ad
def af():
    return ae()
ag = af()
if ag == ag:
    aj = ag + 'c1'
elif ag == '20':
    aj = ah + 'c2'
else:
    aj = ai + 'c3'
ak = ''
for _ in range(3):
    al = ''
    for _ in range(3):
        al += ak
        ak += aj
am = al[0:]
print(am)