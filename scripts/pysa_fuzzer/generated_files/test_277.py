import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 5:
            continue
        b += a
c = ''
for _ in range(4):
    for __ in range(4):
                c += b
d_set = {c, c, c, c}
d = random.choice(list(d_set))
e = ''
countere = 0
while countere < 2:
    e += d
    countere += 1
f = [e for _ in range(6)]
random.shuffle(f)
g = random.choice(f)
h = ''
for _ in range(5):
    i = ''
    for _ in range(5):
        j = ''
        for _ in range(5):
            j += i
            i += h
        h += g
k = j + '4'
l = k + '8'
m = (l, l, l)
n, o, p = m
q = n + o + p
r = ''
for _ in range(5):
    for __ in range(2):
                r += q
s_dict = {81: r, 67: r, 34: r}
t_dict = {5: s_dict, 17: s_dict, 58: s_dict}
u_dict = {90: t_dict, 99: t_dict, 17: t_dict, 29: t_dict, 64: t_dict, 1: t_dict, 8: t_dict, 23: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y_dict = {14: x, 88: x, 8: x, 74: x, 77: x, 32: x, 40: x, 84: x, 42: x, 65: x}
z_dict = {97: y_dict, 21: y_dict, 10: y_dict, 67: y_dict, 68: y_dict, 47: y_dict, 24: y_dict, 2: y_dict, 15: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
def ac():
    return ab
def ad():
    return ac()
ae = ad()
if ae == ae:
    ah = ae + 'c1'
elif ae == '19':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = ah + '.'
def aj():
    return ai
def ak():
    return aj()
al = ak()
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
if aq == aq:
    au = aq + 'c1'
elif aq == '11':
    au = ar + 'c2'
else:
    au = at + 'c3'
av = [au for _ in range(10)]
random.shuffle(av)
aw = random.choice(av)
print(aw)