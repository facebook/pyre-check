import random
import math
a = input()
b = a[0:]
c = b + '.'
d = ''
for _ in range(8):
        if _ == 5:
            break
        d += c
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
def g():
    return f
def h():
    return g()
i = h()
j_set = {i, i, i, i, i, i}
j = random.choice(list(j_set))
k_list = [j for _ in range(7)]
l = random.choice(k_list)
m = ''
for _ in range(2):
    for __ in range(5):
                m += l
if m == '7':
    n = m + ' c1'
elif m == '16':
    n = m + ' c2'
else:
    n = m + ' c3'
o_list = [n for _ in range(2)]
p_list = [o_list for _ in range(10)]
q_list = [p_list for _ in range(9)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
def u():
    return t
v = u()
w = f'string {v}'
x = ''
for _ in range(3):
    y = ''
    for _ in range(3):
        z = ''
        for _ in range(2):
            z += y
            y += x
        x += w
aa = ''
counteraa = 0
while counteraa < 4:
    ab = ''
    counterab = 0
    while counterab < 2:
        ac = ''
        counterac = 0
        while counterac < 4:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = ac[0:]
ae_set = {ad, ad}
ae = random.choice(list(ae_set))
def af():
    return ae
ag = af()
ah = ag + '2'
print(ah)