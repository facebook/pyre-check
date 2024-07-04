import random
import math
a = input()
b_dict = {32: a, 32: a}
c = random.choice(list(b_dict.values()))
if c == '9':
    d = c + ' c1'
elif c == '19':
    d = c + ' c2'
else:
    d = c + ' c3'
e = d + '.'
f_dict = {57: e, 31: e, 10: e, 97: e, 97: e, 70: e, 18: e, 38: e, 65: e}
g_dict = {7: f_dict, 30: f_dict, 9: f_dict, 63: f_dict, 71: f_dict, 87: f_dict, 65: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(4):
    k = ''
    for _ in range(3):
        k += j
        j += i
l = k + '.'
m = ''
for _ in range(3):
    for __ in range(2):
                m += l
n = ''
for _ in range(10):
        if _ == 4:
            continue
        n += m
o = ''
countero = 0
while countero < 5:
    p = ''
    counterp = 0
    while counterp < 5:
        p += o
        counterp += 1
        o += n
        countero += 1
q = ''
counterq = 0
while counterq < 3:
    q += p
    counterq += 1
r = [q for _ in range(6)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(4):
    for __ in range(4):
                t += s
u = t + '5'
v = u + '2'
w = v + '7'
x_dict = {11: w, 97: w, 77: w, 36: w, 38: w, 31: w}
y_dict = {33: x_dict, 84: x_dict, 99: x_dict, 67: x_dict, 95: x_dict, 1: x_dict, 79: x_dict, 41: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = f'string {aa}'
ac_list = [ab for _ in range(6)]
ad_list = [ac_list for _ in range(10)]
ae_list = [ad_list for _ in range(9)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = random.choice(ag)
ai = ah + '.'
aj_dict = {13: ai, 11: ai, 90: ai, 24: ai, 77: ai, 69: ai}
ak_dict = {80: aj_dict, 87: aj_dict, 44: aj_dict, 61: aj_dict, 10: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
print(am)