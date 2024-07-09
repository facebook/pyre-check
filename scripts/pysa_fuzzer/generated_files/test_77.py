import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(5):
        c += b
        b += a
d = [c for _ in range(9)]
random.shuffle(d)
e = random.choice(d)
if e == e:
    h = e + 'c1'
elif e == '15':
    h = f + 'c2'
else:
    h = g + 'c3'
i = ''
for _ in range(2):
    j = ''
    for _ in range(5):
        j += i
        i += h
k = ''
for _ in range(6):
        if _ == 1:
            break
        k += j
l = ''
for _ in range(5):
    m = ''
    for _ in range(5):
        n = ''
        for _ in range(2):
            n += m
            m += l
        l += k
o = f'string {n}'
p = f'string {o}'
q = ''
for _ in range(5):
        if _ == 3:
            break
        q += p
r = f'string {q}'
s_set = {r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = [s for _ in range(5)]
random.shuffle(t)
u = random.choice(t)
v = u[0:]
w = f'string {v}'
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae_list = [ad for _ in range(3)]
af_list = [ae_list for _ in range(10)]
ag = random.choice(af_list)
ah = random.choice(ag)
ai = ah + '7'
aj = ai + '7'
ak = aj + '6'
print(ak)