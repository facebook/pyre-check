import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d_dict = {88: c, 95: c, 90: c, 51: c, 55: c}
e_dict = {11: d_dict, 5: d_dict, 40: d_dict, 60: d_dict, 62: d_dict, 8: d_dict, 80: d_dict, 46: d_dict, 48: d_dict, 52: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = ''
for _ in range(5):
    for __ in range(3):
                h += g
i_set = {h, h, h, h, h}
i = random.choice(list(i_set))
def j():
    return i
def k():
    return j()
l = k()
m = ''
for _ in range(2):
    m += l
n = m[0:]
o_list = [n for _ in range(9)]
p_list = [o_list for _ in range(10)]
q_list = [p_list for _ in range(4)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
u = ''
for _ in range(2):
    v = ''
    for _ in range(4):
        v += u
        u += t
def w():
    return v
def x():
    return w()
y = x()
if y == y:
    ab = y + 'c1'
elif y == '16':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac_dict = {59: ab, 23: ab, 6: ab}
ad_dict = {62: ac_dict, 32: ac_dict, 71: ac_dict, 38: ac_dict, 53: ac_dict}
ae_dict = {84: ad_dict, 35: ad_dict, 9: ad_dict, 69: ad_dict, 71: ad_dict, 76: ad_dict, 49: ad_dict, 56: ad_dict, 36: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
def ai():
    return ah
def aj():
    return ai()
def ak():
    return aj()
al = ak()
am = ''
for _ in range(5):
    for __ in range(3):
                am += al
an = am + '5'
ao = ''
for _ in range(10):
        if _ == 4:
            continue
        ao += an
ap_list = [ao for _ in range(10)]
aq = random.choice(ap_list)
ar = f'string {aq}'
print(ar)