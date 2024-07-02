import random
import math
a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(4):
    d += c
e = ''
for _ in range(10):
        if _ == 2:
            continue
        e += d
f = ''
for _ in range(5):
    for __ in range(5):
                f += e
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = f'string {h}'
def j():
    return i
k = j()
l = ''
for _ in range(2):
    for __ in range(3):
                l += k
m = ''
for _ in range(5):
    for __ in range(2):
                m += l
n = m + '5'
o = n + '3'
p = o + '4'
q = f'string {p}'
r = q + '.'
s = r + '.'
t = f'string {s}'
u = t + '.'
v_dict = {97: u, 3: u, 4: u, 27: u, 82: u, 100: u, 53: u, 55: u}
w_dict = {70: v_dict, 86: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = ''
counterz = 0
while counterz < 2:
    z += y
    counterz += 1
if z == '8':
    aa = z + ' c1'
elif z == '11':
    aa = z + ' c2'
else:
    aa = z + ' c3'
print(aa)