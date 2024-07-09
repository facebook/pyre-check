import random
import math

a = input()
b = ''
for _ in range(4):
    b += a
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d_list = [c for _ in range(6)]
e_list = [d_list for _ in range(5)]
f = random.choice(e_list)
g = random.choice(f)
h_set = {g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = h + '2'
j = i + '8'
k = ''
for _ in range(3):
    l = ''
    for _ in range(3):
        l += k
        k += j
m = ''
for _ in range(4):
    m += l
n_dict = {90: m, 1: m, 13: m, 51: m, 71: m, 33: m, 87: m, 84: m}
o = random.choice(list(n_dict.values()))
p = o[0:]
q = (p, p, p)
r, s, t = q
u = r + s + t
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z_dict = {60: y, 39: y, 90: y, 80: y, 33: y, 2: y, 56: y, 71: y, 65: y, 91: y}
aa = random.choice(list(z_dict.values()))
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae = ad[0:]
af = ae + '7'
ag = af + '2'
ah_dict = {46: ag, 31: ag, 26: ag, 69: ag, 28: ag, 82: ag}
ai_dict = {53: ah_dict, 17: ah_dict, 32: ah_dict, 96: ah_dict, 52: ah_dict, 5: ah_dict, 31: ah_dict, 33: ah_dict, 58: ah_dict, 18: ah_dict}
aj_dict = {25: ai_dict, 68: ai_dict, 55: ai_dict, 23: ai_dict, 80: ai_dict, 32: ai_dict, 7: ai_dict, 10: ai_dict, 60: ai_dict, 59: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an_set = {am, am, am, am, am, am, am, am}
an = random.choice(list(an_set))
ao_dict = {5: an, 63: an, 37: an, 89: an}
ap_dict = {68: ao_dict, 58: ao_dict, 3: ao_dict, 13: ao_dict, 76: ao_dict, 72: ao_dict, 90: ao_dict, 11: ao_dict, 47: ao_dict, 94: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
print(ar)