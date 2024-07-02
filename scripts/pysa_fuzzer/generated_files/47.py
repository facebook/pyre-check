import random
import math
a = input()
b = a + '.'
c = b[0:]
d = c[0:]
e = [d for _ in range(6)]
random.shuffle(e)
f = random.choice(e)
g = f[0:]
h_set = {g, g, g, g}
h = random.choice(list(h_set))
i = (h, h, h)
j, k, l = i
m = j + k + l
if m == '3':
    n = m + ' c1'
elif m == '13':
    n = m + ' c2'
else:
    n = m + ' c3'
o_list = [n for _ in range(6)]
p_list = [o_list for _ in range(3)]
q = random.choice(p_list)
r = random.choice(q)
s = [r for _ in range(10)]
random.shuffle(s)
t = random.choice(s)
u = t + '8'
v = u + '7'
w = v + '7'
if w == '3':
    x = w + ' c1'
elif w == '18':
    x = w + ' c2'
else:
    x = w + ' c3'
if x == '6':
    y = x + ' c1'
elif x == '17':
    y = x + ' c2'
else:
    y = x + ' c3'
z = ''
counterz = 0
while counterz < 5:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        ab = ''
        counterab = 0
        while counterab < 4:
            ab += aa
            counterab += 1
            aa += z
            counteraa += 1
        z += y
        counterz += 1
ac_list = [ab for _ in range(4)]
ad = random.choice(ac_list)
ae_list = [ad for _ in range(9)]
af_list = [ae_list for _ in range(6)]
ag = random.choice(af_list)
ah = random.choice(ag)
ai = ''
for _ in range(10):
        if _ == 3:
            continue
        ai += ah
aj = f'string {ai}'
print(aj)