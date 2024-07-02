import random
import math
a = input()
b_dict = {74: a, 23: a, 50: a, 39: a, 75: a, 34: a, 87: a, 95: a, 20: a, 94: a}
c = random.choice(list(b_dict.values()))
d = c + '.'
e_set = {d, d, d}
e = random.choice(list(e_set))
if e == '9':
    f = e + ' c1'
elif e == '20':
    f = e + ' c2'
else:
    f = e + ' c3'
def g():
    return f
h = g()
def i():
    return h
def j():
    return i()
k = j()
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
for _ in range(8):
        if _ == 1:
            break
        q += p
r = (q, q, q)
s, t, u = r
v = s + t + u
w_set = {v, v, v, v, v, v, v}
w = random.choice(list(w_set))
def x():
    return w
def y():
    return x()
z = y()
aa = f'string {z}'
ab = ''
counterab = 0
while counterab < 2:
    ac = ''
    counterac = 0
    while counterac < 2:
        ad = ''
        counterad = 0
        while counterad < 2:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ad[0:]
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak_set = {aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al_set = {ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
am_set = {al, al, al, al, al, al, al}
am = random.choice(list(am_set))
print(am)