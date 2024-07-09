import random
import math

a = input()
b = ''
for _ in range(8):
        if _ == 3:
            continue
        b += a
c = ''
for _ in range(5):
    for __ in range(2):
                c += b
d_set = {c, c, c, c, c}
d = random.choice(list(d_set))
e = d + '.'
f_dict = {85: e, 85: e, 32: e, 70: e, 83: e, 88: e}
g_dict = {91: f_dict, 81: f_dict, 93: f_dict, 31: f_dict, 83: f_dict}
h_dict = {49: g_dict, 72: g_dict, 76: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = ''
for _ in range(4):
    for __ in range(5):
                l += k
def m():
    return l
n = m()
o = ''
for _ in range(6):
        if _ == 1:
            break
        o += n
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r = q + '8'
s = r + '9'
t_dict = {100: s, 71: s, 50: s, 75: s, 4: s, 54: s, 8: s}
u_dict = {15: t_dict, 82: t_dict, 18: t_dict, 31: t_dict, 25: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x_list = [w for _ in range(9)]
y_list = [x_list for _ in range(6)]
z_list = [y_list for _ in range(7)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
ad = ac + '2'
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
if ah == ah:
    ak = ah + 'c1'
elif ah == '11':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al = ''
for _ in range(2):
    for __ in range(3):
                al += ak
am = ''
counteram = 0
while counteram < 5:
    an = ''
    counteran = 0
    while counteran < 5:
        ao = ''
        counterao = 0
        while counterao < 4:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap = ''
for _ in range(7):
        if _ == 5:
            continue
        ap += ao
print(ap)