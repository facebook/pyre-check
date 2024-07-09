import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 2:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = c + '6'
e = d + '7'
f_set = {e, e, e, e, e, e, e, e, e, e}
f = random.choice(list(f_set))
def g():
    return f
def h():
    return g()
i = h()
j = [i for _ in range(9)]
random.shuffle(j)
k = random.choice(j)
l = ''
for _ in range(3):
    for __ in range(3):
                l += k
m_dict = {68: l, 50: l, 78: l, 28: l, 61: l}
n = random.choice(list(m_dict.values()))
o = ''
for _ in range(6):
        if _ == 2:
            break
        o += n
if o == o:
    r = o + 'c1'
elif o == '11':
    r = p + 'c2'
else:
    r = q + 'c3'
s = (r, r, r)
t, u, v = s
w = t + u + v
def x():
    return w
y = x()
z = ''
for _ in range(5):
    for __ in range(4):
                z += y
aa = ''
for _ in range(2):
    for __ in range(2):
                aa += z
ab = aa + '7'
ac = ''
counterac = 0
while counterac < 2:
    ad = ''
    counterad = 0
    while counterad < 4:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = [ad for _ in range(8)]
random.shuffle(ae)
af = random.choice(ae)
ag = af[0:]
ah = ag + '.'
print(ah)