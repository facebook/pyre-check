import random
import math
a = input()
b_list = [a for _ in range(5)]
c_list = [b_list for _ in range(2)]
d = random.choice(c_list)
e = random.choice(d)
f = f'string {e}'
if f == '2':
    g = f + ' c1'
elif f == '16':
    g = f + ' c2'
else:
    g = f + ' c3'
h = ''
for _ in range(5):
    i = ''
    for _ in range(5):
        i += h
        h += g
j = (i, i, i)
k, l, m = j
n = k + l + m
o = [n for _ in range(8)]
random.shuffle(o)
p = random.choice(o)
q = p + '9'
r = q + '8'
s = r + '3'
t_dict = {100: s, 43: s, 20: s, 55: s, 68: s, 4: s}
u = random.choice(list(t_dict.values()))
v_set = {u, u, u, u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w = f'string {v}'
if w == '5':
    x = w + ' c1'
elif w == '13':
    x = w + ' c2'
else:
    x = w + ' c3'
y = ''
countery = 0
while countery < 2:
    z = ''
    counterz = 0
    while counterz < 5:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = z + '4'
ab = ''
for _ in range(3):
    ab += aa
ac = ''
for _ in range(8):
        if _ == 5:
            break
        ac += ab
ad = ''
for _ in range(5):
    ae = ''
    for _ in range(5):
        af = ''
        for _ in range(3):
            af += ae
            ae += ad
        ad += ac
if af == '6':
    ag = af + ' c1'
elif af == '11':
    ag = af + ' c2'
else:
    ag = af + ' c3'
print(ag)