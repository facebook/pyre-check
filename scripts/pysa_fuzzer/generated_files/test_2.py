import random
import math

a = input()
b_list = [a for _ in range(9)]
c_list = [b_list for _ in range(7)]
d = random.choice(c_list)
e = random.choice(d)
f = f'string {e}'
if f == f:
    i = f + 'c1'
elif f == '18':
    i = g + 'c2'
else:
    i = h + 'c3'
j = ''
for _ in range(8):
        if _ == 3:
            continue
        j += i
k = ''
counterk = 0
while counterk < 5:
    l = ''
    counterl = 0
    while counterl < 3:
        l += k
        counterl += 1
        k += j
        counterk += 1
m = [l for _ in range(8)]
random.shuffle(m)
n = random.choice(m)
o = ''
countero = 0
while countero < 4:
    p = ''
    counterp = 0
    while counterp < 5:
        q = ''
        counterq = 0
        while counterq < 3:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
r = ''
for _ in range(8):
        if _ == 2:
            continue
        r += q
s_set = {r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(5):
    for __ in range(5):
                t += s
u = ''
for _ in range(8):
        if _ == 4:
            break
        u += t
v = ''
for _ in range(6):
        if _ == 2:
            continue
        v += u
w_list = [v for _ in range(4)]
x_list = [w_list for _ in range(7)]
y = random.choice(x_list)
z = random.choice(y)
aa = z[0:]
ab_dict = {41: aa, 78: aa, 56: aa, 30: aa}
ac_dict = {15: ab_dict, 77: ab_dict, 94: ab_dict, 97: ab_dict, 20: ab_dict}
ad_dict = {1: ac_dict, 41: ac_dict, 90: ac_dict, 3: ac_dict, 24: ac_dict, 84: ac_dict, 38: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
def ah():
    return ag
ai = ah()
aj = ai + '.'
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
print(ao)