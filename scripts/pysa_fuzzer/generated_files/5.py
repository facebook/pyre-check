import random
import math
a = input()
b = a[0:]
c = f'string {b}'
d_set = {c, c, c, c, c, c}
d = random.choice(list(d_set))
e_list = [d for _ in range(10)]
f = random.choice(e_list)
g = ''
for _ in range(7):
        if _ == 2:
            continue
        g += f
if g == '4':
    h = g + ' c1'
elif g == '16':
    h = g + ' c2'
else:
    h = g + ' c3'
i_list = [h for _ in range(2)]
j_list = [i_list for _ in range(9)]
k_list = [j_list for _ in range(10)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
o = ''
for _ in range(3):
    o += n
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 3:
        q += p
        counterq += 1
        p += o
        counterp += 1
r = q + '.'
s = (r, r, r)
t, u, v = s
w = t + u + v
x = ''
for _ in range(9):
        if _ == 3:
            continue
        x += w
y_dict = {50: x, 39: x, 82: x, 99: x, 59: x, 27: x, 75: x, 30: x}
z_dict = {80: y_dict, 80: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = f'string {ab}'
ad = [ac for _ in range(8)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
for _ in range(5):
    for __ in range(5):
                af += ae
ag = af[0:]
ah = ''
counterah = 0
while counterah < 5:
    ai = ''
    counterai = 0
    while counterai < 4:
        aj = ''
        counteraj = 0
        while counteraj < 2:
            aj += ai
            counteraj += 1
            ai += ah
            counterai += 1
        ah += ag
        counterah += 1
print(aj)