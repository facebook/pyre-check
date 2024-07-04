import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c_dict = {56: b, 17: b, 71: b, 54: b, 30: b, 58: b, 85: b, 49: b, 98: b, 58: b}
d = random.choice(list(c_dict.values()))
e = (d, d, d)
f, g, h = e
i = f + g + h
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
if k == '3':
    l = k + ' c1'
elif k == '14':
    l = k + ' c2'
else:
    l = k + ' c3'
m = (l, l, l)
n, o, p = m
q = n + o + p
r = q + '.'
s = ''
counters = 0
while counters < 4:
    t = ''
    countert = 0
    while countert < 2:
        t += s
        countert += 1
        s += r
        counters += 1
u = t[0:]
v_dict = {58: u, 73: u}
w = random.choice(list(v_dict.values()))
x_dict = {67: w, 17: w}
y_dict = {49: x_dict, 35: x_dict, 6: x_dict, 61: x_dict, 87: x_dict, 100: x_dict, 92: x_dict}
z_dict = {51: y_dict, 71: y_dict, 46: y_dict, 41: y_dict, 69: y_dict, 23: y_dict, 92: y_dict, 23: y_dict, 41: y_dict, 41: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
ad_set = {ac, ac, ac}
ad = random.choice(list(ad_set))
ae_list = [ad for _ in range(5)]
af = random.choice(ae_list)
ag_set = {af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah_set = {ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
if ah == '8':
    ai = ah + ' c1'
elif ah == '12':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
aj_list = [ai for _ in range(6)]
ak = random.choice(aj_list)
al = ak + '9'
print(al)