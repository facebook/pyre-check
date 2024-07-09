import random
import math

a = input()
b = ''
for _ in range(3):
    b += a
c = b + '1'
d = c + '7'
e_dict = {45: d, 34: d, 57: d, 21: d, 51: d, 11: d, 75: d, 5: d}
f_dict = {92: e_dict, 70: e_dict, 18: e_dict, 72: e_dict, 49: e_dict, 87: e_dict, 9: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
    o = ''
    for _ in range(4):
        o += n
        n += m
p = o + '1'
q = p + '3'
r = q + '7'
s = ''
for _ in range(5):
        if _ == 4:
            continue
        s += r
t = s[0:]
u = ''
for _ in range(2):
    for __ in range(5):
                u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = f'string {z}'
if aa == aa:
    ad = aa + 'c1'
elif aa == '14':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae_set = {ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ae[0:]
ag = af + '.'
ah_dict = {67: ag, 36: ag, 61: ag, 33: ag}
ai_dict = {83: ah_dict, 83: ah_dict, 89: ah_dict, 2: ah_dict, 95: ah_dict, 66: ah_dict, 21: ah_dict}
aj_dict = {99: ai_dict, 7: ai_dict, 56: ai_dict, 86: ai_dict, 90: ai_dict, 13: ai_dict, 95: ai_dict, 16: ai_dict, 63: ai_dict, 23: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an_list = [am for _ in range(7)]
ao_list = [an_list for _ in range(8)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar = aq + '3'
at = ar + '9'
print(at)