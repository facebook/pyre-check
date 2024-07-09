import random
import math

a = input()
b = a + '7'
c = b + '4'
d = c + '.'
e = [d for _ in range(8)]
random.shuffle(e)
f = random.choice(e)
g = [f for _ in range(7)]
random.shuffle(g)
h = random.choice(g)
i_set = {h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = i[0:]
k_list = [j for _ in range(8)]
l_list = [k_list for _ in range(10)]
m = random.choice(l_list)
n = random.choice(m)
o = ''
for _ in range(9):
        if _ == 3:
            continue
        o += n
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 5:
        r = ''
        counterr = 0
        while counterr < 2:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = f'string {r}'
t = [s for _ in range(9)]
random.shuffle(t)
u = random.choice(t)
v = ''
counterv = 0
while counterv < 5:
    w = ''
    counterw = 0
    while counterw < 5:
        x = ''
        counterx = 0
        while counterx < 3:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y_set = {x, x, x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z_set = {y, y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa_list = [z for _ in range(7)]
ab_list = [aa_list for _ in range(9)]
ac_list = [ab_list for _ in range(5)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = random.choice(ae)
ag_dict = {42: af, 94: af}
ah = random.choice(list(ag_dict.values()))
ai = ''
for _ in range(2):
    aj = ''
    for _ in range(5):
        aj += ai
        ai += ah
ak_set = {aj, aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
print(ak)