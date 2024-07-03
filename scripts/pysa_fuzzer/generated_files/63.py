import random
import math
a = input()
b_dict = {14: a, 57: a, 11: a, 19: a, 31: a, 33: a}
c = random.choice(list(b_dict.values()))
d = c + '.'
e = ''
for _ in range(10):
        if _ == 1:
            continue
        e += d
f_list = [e for _ in range(3)]
g = random.choice(f_list)
h_dict = {56: g, 89: g, 43: g, 89: g, 4: g, 54: g}
i_dict = {8: h_dict, 66: h_dict, 43: h_dict, 81: h_dict, 29: h_dict}
j_dict = {5: i_dict, 97: i_dict, 43: i_dict, 9: i_dict, 75: i_dict, 93: i_dict, 72: i_dict, 89: i_dict, 51: i_dict, 71: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n = ''
countern = 0
while countern < 3:
    o = ''
    countero = 0
    while countero < 3:
        o += n
        countero += 1
        n += m
        countern += 1
p_list = [o for _ in range(6)]
q_list = [p_list for _ in range(5)]
r_list = [q_list for _ in range(3)]
s = random.choice(r_list)
t = random.choice(s)
u = random.choice(t)
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = z + '5'
ab = aa + '3'
ac = ''
counterac = 0
while counterac < 4:
    ad = ''
    counterad = 0
    while counterad < 3:
        ae = ''
        counterae = 0
        while counterae < 5:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
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
ah_list = [ag for _ in range(8)]
ai_list = [ah_list for _ in range(6)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al_list = [ak for _ in range(6)]
am_list = [al_list for _ in range(10)]
an = random.choice(am_list)
ao = random.choice(an)
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
av_set = {au, au, au, au, au, au, au, au, au, au}
av = random.choice(list(av_set))
if av == '5':
    aw = av + ' c1'
elif av == '20':
    aw = av + ' c2'
else:
    aw = av + ' c3'
if aw == '4':
    ax = aw + ' c1'
elif aw == '15':
    ax = aw + ' c2'
else:
    ax = aw + ' c3'
ay = ax[0:]
print(ay)