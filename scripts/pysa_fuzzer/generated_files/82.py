import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '2'
h = g + '6'
i = ''
counteri = 0
while counteri < 4:
    i += h
    counteri += 1
j_set = {i, i, i, i, i, i, i, i, i, i}
j = random.choice(list(j_set))
k_list = [j for _ in range(6)]
l_list = [k_list for _ in range(8)]
m_list = [l_list for _ in range(8)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
q = f'string {p}'
r = q + '1'
s = r + '6'
t = s + '2'
u = t[0:]
if u == '6':
    v = u + ' c1'
elif u == '13':
    v = u + ' c2'
else:
    v = u + ' c3'
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 3:
        y = ''
        countery = 0
        while countery < 5:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = y + '.'
aa_dict = {80: z, 14: z}
ab_dict = {18: aa_dict, 46: aa_dict, 59: aa_dict, 63: aa_dict, 42: aa_dict, 40: aa_dict, 5: aa_dict, 93: aa_dict, 51: aa_dict, 56: aa_dict}
ac_dict = {37: ab_dict, 73: ab_dict, 32: ab_dict, 32: ab_dict, 11: ab_dict, 81: ab_dict, 61: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
ag = ''
for _ in range(6):
        if _ == 1:
            break
        ag += af
ah = f'string {ag}'
ai = ''
for _ in range(3):
    for __ in range(3):
                ai += ah
aj = f'string {ai}'
ak = aj[0:]
al = ''
for _ in range(4):
    am = ''
    for _ in range(4):
        an = ''
        for _ in range(5):
            an += am
            am += al
        al += ak
print(an)