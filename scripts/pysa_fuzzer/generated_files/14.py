import random
import math
a = input()
b = a + '6'
c = b + '4'
d = c + '5'
e_dict = {41: d, 81: d, 91: d, 45: d}
f_dict = {34: e_dict, 31: e_dict, 55: e_dict, 48: e_dict, 87: e_dict, 36: e_dict, 38: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i_set = {h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j_dict = {42: i, 49: i, 2: i, 37: i, 87: i, 73: i}
k_dict = {13: j_dict, 46: j_dict, 85: j_dict, 38: j_dict, 21: j_dict, 18: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = ''
for _ in range(4):
    for __ in range(3):
                n += m
o_set = {n, n, n, n, n, n, n, n, n, n}
o = random.choice(list(o_set))
p = o + '5'
q = p + '8'
r = q + '3'
s = r + '1'
if s == '8':
    t = s + ' c1'
elif s == '19':
    t = s + ' c2'
else:
    t = s + ' c3'
u = t + '6'
v = u + '6'
w_set = {v, v, v}
w = random.choice(list(w_set))
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
def ag():
    return af
def ah():
    return ag()
def ai():
    return ah()
aj = ai()
ak = ''
for _ in range(2):
    al = ''
    for _ in range(4):
        am = ''
        for _ in range(5):
            am += al
            al += ak
        ak += aj
an = am + '.'
ao_dict = {100: an, 32: an, 27: an, 77: an, 17: an, 29: an, 92: an, 20: an, 12: an}
ap_dict = {50: ao_dict, 24: ao_dict, 7: ao_dict, 93: ao_dict, 58: ao_dict, 34: ao_dict, 30: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
print(ar)