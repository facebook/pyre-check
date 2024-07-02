import random
import math
a = input()
b = ''
for _ in range(4):
    for __ in range(5):
                b += a
def c():
    return b
d = c()
e = f'string {d}'
f = f'string {e}'
g_list = [f for _ in range(4)]
h = random.choice(g_list)
i = h + '.'
j = (i, i, i)
k, l, m = j
n = k + l + m
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
def q():
    return p
def r():
    return q()
def s():
    return r()
t = s()
def u():
    return t
v = u()
w = v[0:]
x_dict = {59: w, 77: w, 91: w, 9: w, 55: w, 10: w, 45: w, 66: w, 62: w}
y_dict = {36: x_dict, 22: x_dict, 31: x_dict, 9: x_dict, 89: x_dict}
z_dict = {38: y_dict, 19: y_dict, 52: y_dict, 40: y_dict, 89: y_dict, 53: y_dict, 81: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
ad = ac[0:]
ae = [ad for _ in range(10)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
counterag = 0
while counterag < 4:
    ah = ''
    counterah = 0
    while counterah < 2:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
if ah == '5':
    ai = ah + ' c1'
elif ah == '17':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = an + '.'
print(ao)