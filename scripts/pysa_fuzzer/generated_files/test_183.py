import random
import math

a = input()
b = a[0:]
c = b + '5'
d = c + '1'
e_list = [d for _ in range(7)]
f_list = [e_list for _ in range(10)]
g = random.choice(f_list)
h = random.choice(g)
i = ''
counteri = 0
while counteri < 4:
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
        i += h
        counteri += 1
l = k + '.'
m = ''
for _ in range(4):
    for __ in range(4):
                m += l
n = ''
for _ in range(4):
    n += m
def o():
    return n
p = o()
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u + '.'
w_dict = {88: v, 30: v, 85: v, 54: v, 7: v, 78: v, 23: v, 24: v, 86: v}
x_dict = {70: w_dict, 30: w_dict, 17: w_dict}
y_dict = {4: x_dict, 22: x_dict, 51: x_dict, 22: x_dict, 94: x_dict, 59: x_dict, 15: x_dict, 2: x_dict, 61: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = [ab for _ in range(9)]
random.shuffle(ac)
ad = random.choice(ac)
ae_list = [ad for _ in range(5)]
af = random.choice(ae_list)
ag = ''
for _ in range(5):
    ag += af
def ah():
    return ag
def ai():
    return ah()
def aj():
    return ai()
ak = aj()
if ak == ak:
    an = ak + 'c1'
elif ak == '12':
    an = al + 'c2'
else:
    an = am + 'c3'
ao = ''
for _ in range(3):
    for __ in range(3):
                ao += an
ap = ao + '.'
print(ap)