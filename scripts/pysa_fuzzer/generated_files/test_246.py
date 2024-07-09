import random
import math

a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = (c, c, c)
e, f, g = d
h = e + f + g
i_list = [h for _ in range(10)]
j_list = [i_list for _ in range(3)]
k_list = [j_list for _ in range(4)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
o = n + '.'
p = ''
for _ in range(9):
        if _ == 3:
            continue
        p += o
q_list = [p for _ in range(8)]
r_list = [q_list for _ in range(3)]
s_list = [r_list for _ in range(6)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = ''
for _ in range(4):
    x = ''
    for _ in range(4):
        y = ''
        for _ in range(2):
            y += x
            x += w
        w += v
z = f'string {y}'
aa = z[0:]
ab_dict = {25: aa, 34: aa, 40: aa, 43: aa, 23: aa, 97: aa, 70: aa, 67: aa, 75: aa, 3: aa}
ac_dict = {96: ab_dict, 67: ab_dict, 21: ab_dict, 35: ab_dict, 30: ab_dict, 97: ab_dict, 72: ab_dict, 39: ab_dict, 90: ab_dict}
ad_dict = {9: ac_dict, 74: ac_dict, 76: ac_dict, 19: ac_dict, 5: ac_dict, 91: ac_dict, 98: ac_dict, 72: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah = ''
for _ in range(5):
    for __ in range(2):
                ah += ag
def ai():
    return ah
def aj():
    return ai()
def ak():
    return aj()
al = ak()
am = [al for _ in range(10)]
random.shuffle(am)
an = random.choice(am)
ao = [an for _ in range(10)]
random.shuffle(ao)
ap = random.choice(ao)
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
def aw():
    return av
ax = aw()
ay = ''
for _ in range(2):
    for __ in range(3):
                ay += ax
az_dict = {37: ay, 33: ay, 28: ay, 49: ay, 11: ay, 6: ay, 24: ay}
ba_dict = {72: az_dict, 27: az_dict, 17: az_dict, 42: az_dict, 84: az_dict, 45: az_dict, 81: az_dict}
bb_dict = {39: ba_dict, 6: ba_dict, 45: ba_dict, 55: ba_dict, 60: ba_dict, 50: ba_dict, 25: ba_dict, 64: ba_dict}
bc = random.choice(list(bb_dict.values()))
bd = random.choice(list(bc.values()))
be = random.choice(list(bd.values()))
print(be)