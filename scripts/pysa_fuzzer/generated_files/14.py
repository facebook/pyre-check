import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = [d for _ in range(7)]
random.shuffle(e)
f = random.choice(e)
g_set = {f, f, f}
g = random.choice(list(g_set))
h = g[0:]
i = (h, h, h)
j, k, l = i
m = j + k + l
n_list = [m for _ in range(7)]
o_list = [n_list for _ in range(7)]
p = random.choice(o_list)
q = random.choice(p)
r = (q, q, q)
s, t, u = r
v = s + t + u
w_dict = {23: v, 25: v, 36: v, 39: v, 38: v, 20: v, 75: v, 94: v, 39: v, 34: v}
x_dict = {36: w_dict, 54: w_dict, 16: w_dict, 44: w_dict, 49: w_dict, 89: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = z[0:]
ab = aa + '.'
ac = ab + '.'
ad = ''
counterad = 0
while counterad < 5:
    ae = ''
    counterae = 0
    while counterae < 5:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
af = ae + '.'
ag = ''
for _ in range(4):
    for __ in range(5):
                ag += af
def ah():
    return ag
ai = ah()
aj = ai[0:]
ak = ''
for _ in range(5):
        if _ == 5:
            continue
        ak += aj
al = ak + '2'
print(al)