import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
counterc = 0
while counterc < 3:
    d = ''
    counterd = 0
    while counterd < 3:
        d += c
        counterd += 1
        c += b
        counterc += 1
e_list = [d for _ in range(4)]
f = random.choice(e_list)
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = ''
for _ in range(5):
    for __ in range(2):
                i += h
j = ''
for _ in range(10):
        if _ == 4:
            break
        j += i
k = ''
for _ in range(6):
        if _ == 5:
            break
        k += j
l = k[0:]
m = l[0:]
def n():
    return m
def o():
    return n()
def p():
    return o()
q = p()
r = (q, q, q)
s, t, u = r
v = s + t + u
w_dict = {46: v, 28: v, 11: v, 46: v, 52: v, 53: v, 33: v}
x_dict = {67: w_dict, 44: w_dict, 82: w_dict, 41: w_dict, 66: w_dict, 27: w_dict}
y_dict = {37: x_dict, 14: x_dict, 63: x_dict, 17: x_dict, 97: x_dict, 60: x_dict, 34: x_dict, 7: x_dict, 49: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = [ab for _ in range(6)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ad + '2'
if ae == '1':
    af = ae + ' c1'
elif ae == '19':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag_list = [af for _ in range(5)]
ah = random.choice(ag_list)
ai = ah + '2'
aj = ai + '1'
ak = aj + '3'
al = ak + '4'
am = al + '3'
an = am + '5'
print(an)