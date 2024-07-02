import random
import math
a = input()
b_set = {a, a}
b = random.choice(list(b_set))
def c():
    return b
def d():
    return c()
e = d()
f = e + '.'
g = f[0:]
if g == '8':
    h = g + ' c1'
elif g == '12':
    h = g + ' c2'
else:
    h = g + ' c3'
i = ''
for _ in range(4):
    i += h
def j():
    return i
k = j()
l_dict = {40: k, 47: k, 22: k, 12: k, 76: k, 96: k, 94: k, 80: k}
m = random.choice(list(l_dict.values()))
n = ''
countern = 0
while countern < 2:
    o = ''
    countero = 0
    while countero < 2:
        p = ''
        counterp = 0
        while counterp < 2:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
def q():
    return p
def r():
    return q()
s = r()
if s == '9':
    t = s + ' c1'
elif s == '15':
    t = s + ' c2'
else:
    t = s + ' c3'
u_list = [t for _ in range(10)]
v_list = [u_list for _ in range(6)]
w = random.choice(v_list)
x = random.choice(w)
y = ''
for _ in range(4):
    z = ''
    for _ in range(2):
        aa = ''
        for _ in range(5):
            aa += z
            z += y
        y += x
ab = ''
for _ in range(3):
    for __ in range(3):
                ab += aa
ac = ''
for _ in range(4):
    for __ in range(5):
                ac += ab
ad = ac + '5'
ae = ad + '2'
af_dict = {65: ae, 77: ae, 87: ae, 60: ae, 49: ae, 90: ae, 14: ae, 20: ae, 9: ae}
ag_dict = {7: af_dict, 44: af_dict, 41: af_dict, 44: af_dict}
ah_dict = {13: ag_dict, 8: ag_dict, 49: ag_dict, 70: ag_dict, 60: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = random.choice(list(aj.values()))
al_dict = {35: ak, 69: ak, 78: ak, 37: ak, 19: ak, 54: ak, 29: ak}
am_dict = {23: al_dict, 65: al_dict, 69: al_dict, 18: al_dict, 4: al_dict, 12: al_dict, 88: al_dict}
an = random.choice(list(am_dict.values()))
ao = random.choice(list(an.values()))
print(ao)