import random
import math
a = input()
b = a + '8'
c_dict = {47: b, 92: b, 10: b, 66: b}
d = random.choice(list(c_dict.values()))
e = d[0:]
f = e + '.'
def g():
    return f
h = g()
i = ''
counteri = 0
while counteri < 5:
    j = ''
    counterj = 0
    while counterj < 2:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = j + '6'
l = k + '8'
m = (l, l, l)
n, o, p = m
q = n + o + p
r = q + '.'
s_set = {r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = f'string {s}'
if t == '7':
    u = t + ' c1'
elif t == '20':
    u = t + ' c2'
else:
    u = t + ' c3'
v_set = {u, u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w = f'string {v}'
x = w + '.'
y_set = {x, x, x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = ''
counterz = 0
while counterz < 4:
    aa = ''
    counteraa = 0
    while counteraa < 5:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = ''
counterab = 0
while counterab < 5:
    ac = ''
    counterac = 0
    while counterac < 5:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
print(ac)