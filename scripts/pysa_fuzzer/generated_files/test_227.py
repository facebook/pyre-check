import random
import math

a = input()
b_dict = {84: a, 32: a, 20: a, 4: a}
c_dict = {93: b_dict, 86: b_dict, 91: b_dict, 61: b_dict, 57: b_dict, 60: b_dict}
d_dict = {5: c_dict, 93: c_dict, 57: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
j = ''
counterj = 0
while counterj < 3:
    k = ''
    counterk = 0
    while counterk < 5:
        k += j
        counterk += 1
        j += i
        counterj += 1
if k == k:
    n = k + 'c1'
elif k == '15':
    n = l + 'c2'
else:
    n = m + 'c3'
o = ''
for _ in range(3):
    p = ''
    for _ in range(3):
        p += o
        o += n
q = [p for _ in range(9)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(7):
        if _ == 1:
            break
        s += r
t_set = {s, s}
t = random.choice(list(t_set))
u = ''
for _ in range(3):
    for __ in range(4):
                u += t
v_set = {u, u, u, u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w_list = [v for _ in range(8)]
x = random.choice(w_list)
y = x[0:]
def z():
    return y
def aa():
    return z()
ab = aa()
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
def ah():
    return ag
ai = ah()
aj = [ai for _ in range(8)]
random.shuffle(aj)
ak = random.choice(aj)
if ak == ak:
    an = ak + 'c1'
elif ak == '19':
    an = al + 'c2'
else:
    an = am + 'c3'
ao = an + '.'
print(ao)