import random
import math

a = input()
b = a + '.'
c = [b for _ in range(7)]
random.shuffle(c)
d = random.choice(c)
e = [d for _ in range(10)]
random.shuffle(e)
f = random.choice(e)
def g():
    return f
def h():
    return g()
i = h()
j = [i for _ in range(6)]
random.shuffle(j)
k = random.choice(j)
l = [k for _ in range(10)]
random.shuffle(l)
m = random.choice(l)
n_set = {m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o = n + '1'
p = o + '9'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u + '2'
w = v + '4'
x = w + '7'
y = x[0:]
z = y[0:]
if z == z:
    ac = z + 'c1'
elif z == '12':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad_dict = {99: ac, 7: ac, 45: ac, 42: ac, 66: ac, 59: ac}
ae = random.choice(list(ad_dict.values()))
af_dict = {6: ae, 96: ae, 22: ae, 56: ae}
ag_dict = {7: af_dict, 15: af_dict, 66: af_dict, 81: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = ai + '.'
ak = ''
for _ in range(5):
        if _ == 4:
            continue
        ak += aj
al = ''
for _ in range(5):
    am = ''
    for _ in range(4):
        an = ''
        for _ in range(5):
            an += am
            am += al
        al += ak
print(an)