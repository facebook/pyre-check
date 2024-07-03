import random
import math
a = input()
b = a + '.'
c = [b for _ in range(7)]
random.shuffle(c)
d = random.choice(c)
e = ''
for _ in range(8):
        if _ == 4:
            continue
        e += d
f = e[0:]
if f == '1':
    g = f + ' c1'
elif f == '13':
    g = f + ' c2'
else:
    g = f + ' c3'
h_list = [g for _ in range(5)]
i_list = [h_list for _ in range(10)]
j_list = [i_list for _ in range(2)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = ''
for _ in range(5):
    for __ in range(5):
                n += m
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = (p, p, p)
r, s, t = q
u = r + s + t
v_set = {u, u, u, u}
v = random.choice(list(v_set))
w = v + '4'
x = w + '9'
y = x + '4'
z = f'string {y}'
aa = ''
counteraa = 0
while counteraa < 4:
    ab = ''
    counterab = 0
    while counterab < 4:
        ac = ''
        counterac = 0
        while counterac < 4:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ah + '.'
aj = ai + '6'
ak = ''
for _ in range(4):
    ak += aj
al = [ak for _ in range(5)]
random.shuffle(al)
am = random.choice(al)
print(am)