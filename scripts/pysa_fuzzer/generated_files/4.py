import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
for _ in range(6):
        if _ == 3:
            continue
        e += d
f_dict = {94: e, 44: e, 3: e, 89: e, 92: e, 22: e, 83: e, 81: e, 3: e}
g = random.choice(list(f_dict.values()))
h = ''
for _ in range(5):
    h += g
i = ''
for _ in range(2):
    for __ in range(3):
                i += h
j = (i, i, i)
k, l, m = j
n = k + l + m
o = f'string {n}'
p = o[0:]
q = p[0:]
r = ''
for _ in range(8):
        if _ == 2:
            break
        r += q
s = ''
counters = 0
while counters < 3:
    s += r
    counters += 1
t = [s for _ in range(6)]
random.shuffle(t)
u = random.choice(t)
v_set = {u, u}
v = random.choice(list(v_set))
w = ''
for _ in range(2):
    w += v
x = ''
for _ in range(5):
        if _ == 5:
            continue
        x += w
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
aa = ''
counteraa = 0
while counteraa < 4:
    aa += z
    counteraa += 1
ab = ''
for _ in range(2):
    ac = ''
    for _ in range(5):
        ac += ab
        ab += aa
print(ac)