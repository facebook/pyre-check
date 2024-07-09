import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e_list = [d for _ in range(6)]
f_list = [e_list for _ in range(2)]
g_list = [f_list for _ in range(3)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k = f'string {j}'
l = ''
counterl = 0
while counterl < 4:
    l += k
    counterl += 1
m = [l for _ in range(10)]
random.shuffle(m)
n = random.choice(m)
o = n + '8'
p = o + '7'
if p == p:
    s = p + 'c1'
elif p == '12':
    s = q + 'c2'
else:
    s = r + 'c3'
t = s + '8'
u = t + '.'
v = f'string {u}'
w = v[0:]
if w == w:
    z = w + 'c1'
elif w == '16':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = ''
counteraa = 0
while counteraa < 2:
    ab = ''
    counterab = 0
    while counterab < 2:
        ac = ''
        counterac = 0
        while counterac < 5:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
if ac == ac:
    af = ac + 'c1'
elif ac == '19':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag_set = {af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = ''
for _ in range(4):
    ai = ''
    for _ in range(4):
        aj = ''
        for _ in range(4):
            aj += ai
            ai += ah
        ah += ag
if aj == aj:
    am = aj + 'c1'
elif aj == '11':
    am = ak + 'c2'
else:
    am = al + 'c3'
if am == am:
    ap = am + 'c1'
elif am == '18':
    ap = an + 'c2'
else:
    ap = ao + 'c3'
print(ap)