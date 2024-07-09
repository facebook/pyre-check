import random
import math

a = input()
b = ''
for _ in range(2):
    b += a
c = b[0:]
def d():
    return c
def e():
    return d()
f = e()
g_set = {f, f, f, f, f, f, f, f, f}
g = random.choice(list(g_set))
h = ''
for _ in range(2):
    i = ''
    for _ in range(4):
        i += h
        h += g
j = i + '.'
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m = l + '6'
n = m + '3'
def o():
    return n
p = o()
q_list = [p for _ in range(7)]
r_list = [q_list for _ in range(7)]
s_list = [r_list for _ in range(8)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = f'string {v}'
x = ''
for _ in range(3):
    for __ in range(4):
                x += w
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad_dict = {12: ac, 45: ac, 33: ac, 17: ac, 52: ac, 70: ac, 49: ac, 29: ac, 27: ac}
ae_dict = {29: ad_dict, 58: ad_dict, 7: ad_dict, 98: ad_dict, 85: ad_dict, 57: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah_dict = {96: ag, 24: ag, 38: ag, 46: ag}
ai = random.choice(list(ah_dict.values()))
aj = f'string {ai}'
ak_dict = {75: aj, 54: aj, 20: aj, 85: aj, 74: aj, 93: aj, 29: aj, 15: aj}
al = random.choice(list(ak_dict.values()))
am = al + '8'
an = am + '7'
print(an)