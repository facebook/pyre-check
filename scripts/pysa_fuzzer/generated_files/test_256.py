import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '9'
h = g + '.'
i = h + '.'
j_list = [i for _ in range(5)]
k = random.choice(j_list)
l = ''
for _ in range(2):
    for __ in range(4):
                l += k
m_list = [l for _ in range(9)]
n = random.choice(m_list)
o = ''
for _ in range(3):
    p = ''
    for _ in range(5):
        p += o
        o += n
q = p + '.'
r = q + '.'
s = [r for _ in range(5)]
random.shuffle(s)
t = random.choice(s)
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y + '.'
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = f'string {ae}'
ag_dict = {30: af, 62: af, 30: af, 99: af, 29: af, 45: af, 54: af}
ah_dict = {48: ag_dict, 75: ag_dict, 78: ag_dict, 84: ag_dict, 39: ag_dict, 97: ag_dict}
ai_dict = {16: ah_dict, 39: ah_dict, 30: ah_dict, 99: ah_dict, 97: ah_dict, 53: ah_dict, 22: ah_dict, 57: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am = [al for _ in range(10)]
random.shuffle(am)
an = random.choice(am)
ao = [an for _ in range(9)]
random.shuffle(ao)
ap = random.choice(ao)
print(ap)