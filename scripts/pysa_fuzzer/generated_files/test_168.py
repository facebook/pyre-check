import random
import math

a = input()
b = a + '2'
c = b + '2'
d = c + '7'
e = d + '1'
f = e + '7'
g = ''
counterg = 0
while counterg < 4:
    h = ''
    counterh = 0
    while counterh < 3:
        i = ''
        counteri = 0
        while counteri < 3:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
j = f'string {i}'
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 2:
        m = ''
        counterm = 0
        while counterm < 5:
            m += l
            counterm += 1
            l += k
            counterl += 1
        k += j
        counterk += 1
if m == m:
    p = m + 'c1'
elif m == '14':
    p = n + 'c2'
else:
    p = o + 'c3'
q_set = {p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
def r():
    return q
s = r()
t = f'string {s}'
u = ''
for _ in range(4):
    for __ in range(3):
                u += t
v = f'string {u}'
w = f'string {v}'
x = w + '.'
y = [x for _ in range(8)]
random.shuffle(y)
z = random.choice(y)
aa = ''
for _ in range(5):
    ab = ''
    for _ in range(5):
        ab += aa
        aa += z
ac = f'string {ab}'
if ac == ac:
    af = ac + 'c1'
elif ac == '18':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
print(ak)