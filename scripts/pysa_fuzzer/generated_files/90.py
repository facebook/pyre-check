import random
import math
a = input()
b_dict = {18: a, 76: a, 72: a, 91: a, 91: a, 29: a}
c_dict = {36: b_dict, 63: b_dict, 88: b_dict, 42: b_dict}
d_dict = {81: c_dict, 26: c_dict, 68: c_dict, 61: c_dict, 86: c_dict, 64: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = [g for _ in range(10)]
random.shuffle(h)
i = random.choice(h)
j = [i for _ in range(7)]
random.shuffle(j)
k = random.choice(j)
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
p = [o for _ in range(8)]
random.shuffle(p)
q = random.choice(p)
r_list = [q for _ in range(5)]
s = random.choice(r_list)
t = ''
countert = 0
while countert < 5:
    t += s
    countert += 1
u = ''
counteru = 0
while counteru < 5:
    v = ''
    counterv = 0
    while counterv < 5:
        w = ''
        counterw = 0
        while counterw < 3:
            w += v
            counterw += 1
            v += u
            counterv += 1
        u += t
        counteru += 1
x = w + '6'
y = x + '2'
z = y + '7'
aa = z[0:]
ab = ''
counterab = 0
while counterab < 5:
    ac = ''
    counterac = 0
    while counterac < 3:
        ad = ''
        counterad = 0
        while counterad < 3:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ''
for _ in range(2):
    af = ''
    for _ in range(5):
        ag = ''
        for _ in range(3):
            ag += af
            af += ae
        ae += ad
ah_dict = {33: ag, 3: ag, 67: ag, 20: ag, 39: ag, 31: ag}
ai = random.choice(list(ah_dict.values()))
aj_dict = {49: ai, 83: ai}
ak = random.choice(list(aj_dict.values()))
al = ''
for _ in range(4):
    for __ in range(2):
                al += ak
am = [al for _ in range(6)]
random.shuffle(am)
an = random.choice(am)
if an == '1':
    ao = an + ' c1'
elif an == '20':
    ao = an + ' c2'
else:
    ao = an + ' c3'
ap_set = {ao, ao, ao, ao, ao, ao, ao, ao, ao, ao}
ap = random.choice(list(ap_set))
print(ap)