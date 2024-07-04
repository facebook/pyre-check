import random
import math
a = input()
b = f'string {a}'
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
def d():
    return c
def e():
    return d()
def f():
    return e()
g = f()
h_dict = {99: g, 92: g}
i_dict = {43: h_dict, 27: h_dict, 68: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
def l():
    return k
m = l()
if m == '6':
    n = m + ' c1'
elif m == '16':
    n = m + ' c2'
else:
    n = m + ' c3'
o = ''
for _ in range(2):
    for __ in range(4):
                o += n
p = o + '.'
q = p + '.'
r_dict = {40: q, 50: q, 2: q, 51: q, 83: q, 21: q, 70: q, 73: q, 75: q, 60: q}
s_dict = {95: r_dict, 49: r_dict, 13: r_dict, 70: r_dict, 38: r_dict, 54: r_dict, 21: r_dict, 20: r_dict, 65: r_dict, 84: r_dict}
t_dict = {54: s_dict, 71: s_dict, 52: s_dict, 35: s_dict, 9: s_dict, 48: s_dict, 49: s_dict, 38: s_dict, 42: s_dict, 31: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = f'string {w}'
y_list = [x for _ in range(10)]
z = random.choice(y_list)
aa = ''
for _ in range(5):
        if _ == 5:
            break
        aa += z
if aa == '6':
    ab = aa + ' c1'
elif aa == '19':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac = f'string {ab}'
ad = f'string {ac}'
ae = ad[0:]
def af():
    return ae
def ag():
    return af()
ah = ag()
print(ah)