import random
import math
a = input()
b_dict = {23: a, 51: a, 50: a, 8: a}
c_dict = {34: b_dict, 56: b_dict, 65: b_dict, 48: b_dict, 45: b_dict, 40: b_dict, 61: b_dict, 52: b_dict, 98: b_dict}
d_dict = {92: c_dict, 69: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = [g for _ in range(10)]
random.shuffle(h)
i = random.choice(h)
j = [i for _ in range(8)]
random.shuffle(j)
k = random.choice(j)
l_set = {k, k, k, k, k, k, k}
l = random.choice(list(l_set))
def m():
    return l
def n():
    return m()
o = n()
def p():
    return o
def q():
    return p()
r = q()
s = r + '.'
t = s + '7'
u = t + '1'
v = u + '1'
w = v[0:]
x = w[0:]
y = x + '.'
z = [y for _ in range(7)]
random.shuffle(z)
aa = random.choice(z)
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae_list = [ad for _ in range(9)]
af = random.choice(ae_list)
ag = af[0:]
ah = ag + '.'
ai_set = {ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
print(ai)