import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(4):
                b += a
c = b + '.'
d = c[0:]
e = [d for _ in range(8)]
random.shuffle(e)
f = random.choice(e)
def g():
    return f
h = g()
i_set = {h, h, h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
def j():
    return i
k = j()
l = ''
for _ in range(2):
    l += k
m = [l for _ in range(5)]
random.shuffle(m)
n = random.choice(m)
o = ''
for _ in range(10):
        if _ == 3:
            break
        o += n
p = ''
for _ in range(9):
        if _ == 4:
            continue
        p += o
q = p[0:]
r = q[0:]
s = r[0:]
t = ''
for _ in range(4):
    for __ in range(3):
                t += s
u_dict = {47: t, 25: t, 28: t}
v_dict = {75: u_dict, 99: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = x[0:]
if y == y:
    ab = y + 'c1'
elif y == '11':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
print(ab)