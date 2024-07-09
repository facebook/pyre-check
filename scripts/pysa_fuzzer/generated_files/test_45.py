import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(3):
                b += a
c = b + '8'
d = c + '6'
e = d + '.'
f = ''
counterf = 0
while counterf < 5:
    g = ''
    counterg = 0
    while counterg < 3:
        h = ''
        counterh = 0
        while counterh < 2:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i = f'string {h}'
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
if m == m:
    p = m + 'c1'
elif m == '12':
    p = n + 'c2'
else:
    p = o + 'c3'
q_set = {p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = [q for _ in range(5)]
random.shuffle(r)
s = random.choice(r)
t = s[0:]
u = [t for _ in range(5)]
random.shuffle(u)
v = random.choice(u)
w = v + '.'
x = w + '2'
y = x + '8'
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae_list = [ad for _ in range(2)]
af_list = [ae_list for _ in range(2)]
ag_list = [af_list for _ in range(3)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak_set = {aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al_dict = {50: ak, 93: ak, 50: ak, 42: ak, 19: ak, 6: ak, 7: ak, 57: ak, 48: ak, 76: ak}
am = random.choice(list(al_dict.values()))
an = f'string {am}'
print(an)