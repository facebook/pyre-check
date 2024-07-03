import random
import math
a = input()
b_dict = {46: a, 32: a, 1: a, 22: a, 48: a, 1: a, 61: a, 62: a, 95: a}
c = random.choice(list(b_dict.values()))
d = c + '3'
e = ''
for _ in range(5):
        if _ == 1:
            break
        e += d
f = [e for _ in range(9)]
random.shuffle(f)
g = random.choice(f)
h = (g, g, g)
i, j, k = h
l = i + j + k
m = l + '6'
n = m + '3'
o = ''
for _ in range(10):
        if _ == 5:
            continue
        o += n
if o == '3':
    p = o + ' c1'
elif o == '18':
    p = o + ' c2'
else:
    p = o + ' c3'
q = p + '.'
r = q + '8'
s_set = {r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(6):
        if _ == 4:
            continue
        t += s
u = ''
for _ in range(9):
        if _ == 1:
            continue
        u += t
v = ''
for _ in range(8):
        if _ == 1:
            continue
        v += u
w = [v for _ in range(7)]
random.shuffle(w)
x = random.choice(w)
y_dict = {74: x, 96: x, 64: x, 9: x, 60: x, 51: x, 85: x}
z = random.choice(list(y_dict.values()))
aa = ''
counteraa = 0
while counteraa < 2:
    aa += z
    counteraa += 1
ab = ''
for _ in range(2):
    ac = ''
    for _ in range(3):
        ac += ab
        ab += aa
print(ac)