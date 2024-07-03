import random
import math
a = input()
b_set = {a, a}
b = random.choice(list(b_set))
c = b + '8'
d = c + '7'
e = d + '2'
f_dict = {24: e, 98: e, 10: e}
g = random.choice(list(f_dict.values()))
h = g + '.'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(6):
        if _ == 2:
            break
        n += m
if n == '10':
    o = n + ' c1'
elif n == '11':
    o = n + ' c2'
else:
    o = n + ' c3'
if o == '8':
    p = o + ' c1'
elif o == '16':
    p = o + ' c2'
else:
    p = o + ' c3'
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
s = (r, r, r)
t, u, v = s
w = t + u + v
x = f'string {w}'
y = f'string {x}'
z = f'string {y}'
aa_dict = {17: z, 17: z}
ab_dict = {94: aa_dict, 48: aa_dict, 15: aa_dict, 40: aa_dict, 99: aa_dict, 28: aa_dict, 97: aa_dict, 6: aa_dict}
ac_dict = {4: ab_dict, 83: ab_dict, 60: ab_dict, 15: ab_dict, 60: ab_dict, 38: ab_dict, 83: ab_dict, 44: ab_dict, 41: ab_dict, 39: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
def al():
    return ak
am = al()
an = ''
for _ in range(3):
    for __ in range(3):
                an += am
ao = an + '.'
print(ao)