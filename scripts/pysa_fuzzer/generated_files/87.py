import random
import math
a = input()
b_dict = {49: a, 96: a, 47: a, 1: a, 88: a}
c_dict = {2: b_dict, 23: b_dict, 46: b_dict, 62: b_dict, 37: b_dict, 62: b_dict, 73: b_dict, 97: b_dict, 35: b_dict}
d_dict = {89: c_dict, 45: c_dict, 100: c_dict, 11: c_dict, 61: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = (g, g, g)
i, j, k = h
l = i + j + k
def m():
    return l
n = m()
o_list = [n for _ in range(9)]
p_list = [o_list for _ in range(4)]
q_list = [p_list for _ in range(6)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
u = t[0:]
v = [u for _ in range(5)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(3):
    y = ''
    for _ in range(4):
        y += x
        x += w
z_list = [y for _ in range(8)]
aa_list = [z_list for _ in range(8)]
ab_list = [aa_list for _ in range(4)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 5:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah = [ag for _ in range(9)]
random.shuffle(ah)
ai = random.choice(ah)
aj = ''
for _ in range(4):
    for __ in range(3):
                aj += ai
if aj == '3':
    ak = aj + ' c1'
elif aj == '13':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
def al():
    return ak
am = al()
an_set = {am, am, am, am, am, am, am}
an = random.choice(list(an_set))
ao_set = {an, an, an, an, an, an, an, an, an}
ao = random.choice(list(ao_set))
ap = ao + '.'
aq = ap + '2'
ar = aq + '1'
at = ar + '4'
au_list = [at for _ in range(9)]
av_list = [au_list for _ in range(5)]
aw_list = [av_list for _ in range(4)]
ax = random.choice(aw_list)
ay = random.choice(ax)
az = random.choice(ay)
print(az)