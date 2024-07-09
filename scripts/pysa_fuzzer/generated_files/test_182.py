import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '19':
    d = b + 'c2'
else:
    d = c + 'c3'
e = f'string {d}'
f_dict = {20: e, 92: e, 16: e, 94: e, 68: e, 28: e, 38: e, 79: e}
g = random.choice(list(f_dict.values()))
h_list = [g for _ in range(4)]
i_list = [h_list for _ in range(9)]
j_list = [i_list for _ in range(10)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n_list = [m for _ in range(6)]
o = random.choice(n_list)
p_set = {o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = f'string {p}'
r = [q for _ in range(7)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(2):
    u = ''
    for _ in range(2):
        v = ''
        for _ in range(4):
            v += u
            u += t
        t += s
w = v + '8'
x = w + '5'
y = x + '6'
z_list = [y for _ in range(6)]
aa_list = [z_list for _ in range(7)]
ab_list = [aa_list for _ in range(8)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
af = ae + '8'
ag = af + '3'
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am = al + '.'
an = am + '.'
ao = ''
for _ in range(8):
        if _ == 3:
            break
        ao += an
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
av = ''
counterav = 0
while counterav < 3:
    aw = ''
    counteraw = 0
    while counteraw < 3:
        ax = ''
        counterax = 0
        while counterax < 2:
            ax += aw
            counterax += 1
            aw += av
            counteraw += 1
        av += au
        counterav += 1
print(ax)