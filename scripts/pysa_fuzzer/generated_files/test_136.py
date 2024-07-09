import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 2:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e_dict = {23: d, 100: d, 64: d, 31: d, 31: d, 85: d}
f_dict = {21: e_dict, 48: e_dict, 81: e_dict, 13: e_dict, 73: e_dict, 83: e_dict, 50: e_dict, 75: e_dict, 8: e_dict}
g_dict = {34: f_dict, 26: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k_dict = {93: j, 35: j, 91: j, 97: j, 32: j, 45: j}
l = random.choice(list(k_dict.values()))
m = ''
for _ in range(2):
    for __ in range(2):
                m += l
n_dict = {84: m, 73: m, 99: m, 36: m, 90: m, 13: m}
o = random.choice(list(n_dict.values()))
p_set = {o, o, o, o, o, o}
p = random.choice(list(p_set))
q_dict = {3: p, 8: p, 10: p}
r_dict = {8: q_dict, 40: q_dict, 5: q_dict, 27: q_dict, 52: q_dict}
s_dict = {48: r_dict, 8: r_dict, 44: r_dict, 44: r_dict, 60: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = random.choice(list(u.values()))
w_set = {v, v}
w = random.choice(list(w_set))
x = w[0:]
if x == x:
    aa = x + 'c1'
elif x == '16':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab = aa + '.'
ac_set = {ab, ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ac + '.'
ae_dict = {15: ad, 55: ad, 23: ad, 43: ad, 25: ad, 93: ad, 33: ad, 28: ad, 12: ad, 5: ad}
af_dict = {71: ae_dict, 85: ae_dict}
ag_dict = {84: af_dict, 44: af_dict, 9: af_dict, 56: af_dict, 77: af_dict, 57: af_dict, 71: af_dict, 17: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak = aj[0:]
al = ak[0:]
am = ''
for _ in range(8):
        if _ == 2:
            continue
        am += al
an = ''
for _ in range(10):
        if _ == 4:
            continue
        an += am
print(an)