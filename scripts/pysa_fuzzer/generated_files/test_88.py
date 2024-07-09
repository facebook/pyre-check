import random
import math

a = input()
b_list = [a for _ in range(9)]
c = random.choice(b_list)
d = ''
counterd = 0
while counterd < 3:
    e = ''
    countere = 0
    while countere < 3:
        e += d
        countere += 1
        d += c
        counterd += 1
f = (e, e, e)
g, h, i = f
j = g + h + i
k = ''
for _ in range(6):
        if _ == 2:
            break
        k += j
l_list = [k for _ in range(3)]
m = random.choice(l_list)
n_set = {m, m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o_list = [n for _ in range(6)]
p_list = [o_list for _ in range(5)]
q = random.choice(p_list)
r = random.choice(q)
s_dict = {61: r, 43: r, 64: r, 37: r, 17: r, 91: r}
t_dict = {95: s_dict, 67: s_dict, 79: s_dict}
u_dict = {54: t_dict, 58: t_dict, 71: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = f'string {x}'
z = y + '.'
aa_dict = {93: z, 100: z}
ab = random.choice(list(aa_dict.values()))
ac = ab[0:]
ad = ''
for _ in range(6):
        if _ == 1:
            continue
        ad += ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = f'string {ai}'
ak = aj[0:]
if ak == ak:
    an = ak + 'c1'
elif ak == '18':
    an = al + 'c2'
else:
    an = am + 'c3'
ao = [an for _ in range(10)]
random.shuffle(ao)
ap = random.choice(ao)
print(ap)