import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 2:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = ''
for _ in range(9):
        if _ == 5:
            continue
        e += d
f = e + '.'
g = f + '7'
h = g + '6'
i = f'string {h}'
j = i + '.'
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m = (l, l, l)
n, o, p = m
q = n + o + p
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = u + '.'
w = ''
for _ in range(7):
        if _ == 4:
            break
        w += v
if w == '4':
    x = w + ' c1'
elif w == '11':
    x = w + ' c2'
else:
    x = w + ' c3'
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
if z == '1':
    aa = z + ' c1'
elif z == '13':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_dict = {4: aa, 69: aa, 99: aa, 83: aa}
ac_dict = {55: ab_dict, 15: ab_dict, 30: ab_dict, 67: ab_dict, 44: ab_dict}
ad_dict = {37: ac_dict, 94: ac_dict, 47: ac_dict, 47: ac_dict, 58: ac_dict, 83: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah_list = [ag for _ in range(3)]
ai_list = [ah_list for _ in range(2)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = f'string {ak}'
am = ''
for _ in range(6):
        if _ == 3:
            break
        am += al
print(am)