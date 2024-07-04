import random
import math
a = input()
b_dict = {30: a, 25: a, 80: a, 93: a, 36: a, 80: a}
c_dict = {37: b_dict, 33: b_dict, 36: b_dict, 93: b_dict, 52: b_dict, 14: b_dict, 28: b_dict, 93: b_dict, 19: b_dict, 50: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f = [e for _ in range(5)]
random.shuffle(f)
g = random.choice(f)
h = g + '2'
i = h + '5'
j = ''
for _ in range(8):
        if _ == 1:
            break
        j += i
k = ''
for _ in range(10):
        if _ == 1:
            break
        k += j
l = f'string {k}'
m = ''
for _ in range(3):
    for __ in range(5):
                m += l
n_list = [m for _ in range(2)]
o_list = [n_list for _ in range(6)]
p = random.choice(o_list)
q = random.choice(p)
r_dict = {47: q, 98: q, 46: q, 13: q, 69: q, 68: q}
s_dict = {98: r_dict, 5: r_dict, 44: r_dict, 10: r_dict, 70: r_dict, 8: r_dict, 26: r_dict, 79: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z = y[0:]
aa = z[0:]
ab = ''
for _ in range(3):
    ac = ''
    for _ in range(4):
        ad = ''
        for _ in range(5):
            ad += ac
            ac += ab
        ab += aa
ae_dict = {97: ad, 30: ad, 22: ad, 64: ad, 52: ad, 36: ad, 91: ad, 2: ad, 5: ad}
af_dict = {47: ae_dict, 97: ae_dict, 18: ae_dict, 62: ae_dict, 100: ae_dict, 11: ae_dict, 16: ae_dict, 23: ae_dict}
ag_dict = {4: af_dict, 61: af_dict, 87: af_dict, 79: af_dict, 91: af_dict, 97: af_dict, 73: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak_set = {aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
if ak == '3':
    al = ak + ' c1'
elif ak == '12':
    al = ak + ' c2'
else:
    al = ak + ' c3'
am = [al for _ in range(6)]
random.shuffle(am)
an = random.choice(am)
ao_dict = {29: an, 64: an, 15: an}
ap_dict = {45: ao_dict, 21: ao_dict, 30: ao_dict, 94: ao_dict, 74: ao_dict, 16: ao_dict, 29: ao_dict, 28: ao_dict, 44: ao_dict, 77: ao_dict}
aq_dict = {14: ap_dict, 62: ap_dict, 4: ap_dict, 47: ap_dict}
ar = random.choice(list(aq_dict.values()))
at = random.choice(list(ar.values()))
au = random.choice(list(at.values()))
print(au)