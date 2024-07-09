import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '15':
    d = b + 'c2'
else:
    d = c + 'c3'
def e():
    return d
def f():
    return e()
def g():
    return f()
h = g()
i_list = [h for _ in range(7)]
j = random.choice(i_list)
k = ''
counterk = 0
while counterk < 2:
    l = ''
    counterl = 0
    while counterl < 4:
        m = ''
        counterm = 0
        while counterm < 5:
            m += l
            counterm += 1
            l += k
            counterl += 1
        k += j
        counterk += 1
n = ''
for _ in range(3):
    for __ in range(4):
                n += m
o = n[0:]
p_set = {o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = p + '5'
r_dict = {18: q, 9: q, 14: q, 4: q, 48: q, 92: q, 73: q, 77: q}
s_dict = {46: r_dict, 84: r_dict, 45: r_dict, 77: r_dict, 50: r_dict, 66: r_dict, 63: r_dict, 17: r_dict}
t_dict = {21: s_dict, 72: s_dict, 97: s_dict, 93: s_dict, 37: s_dict, 58: s_dict, 26: s_dict, 49: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = w[0:]
y = ''
for _ in range(9):
        if _ == 4:
            continue
        y += x
z = y + '9'
aa = z + '1'
ab = aa + '9'
ac_dict = {76: ab, 13: ab, 26: ab, 78: ab, 12: ab, 37: ab}
ad_dict = {15: ac_dict, 37: ac_dict, 46: ac_dict, 53: ac_dict, 32: ac_dict}
ae_dict = {41: ad_dict, 63: ad_dict, 1: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
ai = ''
for _ in range(2):
    for __ in range(3):
                ai += ah
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao_dict = {73: an, 84: an, 44: an, 39: an, 55: an, 36: an}
ap_dict = {69: ao_dict, 5: ao_dict, 35: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
at_dict = {2: ar, 70: ar, 5: ar, 35: ar, 90: ar, 71: ar, 94: ar, 65: ar, 60: ar}
au = random.choice(list(at_dict.values()))
av = au + '9'
print(av)