import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = c[0:]
e_dict = {11: d, 63: d}
f_dict = {37: e_dict, 47: e_dict, 5: e_dict, 80: e_dict, 7: e_dict, 70: e_dict}
g_dict = {98: f_dict, 88: f_dict, 8: f_dict, 17: f_dict, 59: f_dict, 82: f_dict, 29: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = j[0:]
def l():
    return k
def m():
    return l()
n = m()
def o():
    return n
def p():
    return o()
def q():
    return p()
r = q()
s_set = {r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(5):
    for __ in range(2):
                t += s
u = t + '1'
v = u + '2'
w = v + '8'
x = f'string {w}'
y_list = [x for _ in range(6)]
z_list = [y_list for _ in range(6)]
aa = random.choice(z_list)
ab = random.choice(aa)
if ab == ab:
    ae = ab + 'c1'
elif ab == '12':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
if ae == ae:
    ah = ae + 'c1'
elif ae == '16':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = [ah for _ in range(6)]
random.shuffle(ai)
aj = random.choice(ai)
ak = ''
counterak = 0
while counterak < 2:
    al = ''
    counteral = 0
    while counteral < 3:
        am = ''
        counteram = 0
        while counteram < 4:
            am += al
            counteram += 1
            al += ak
            counteral += 1
        ak += aj
        counterak += 1
an_list = [am for _ in range(5)]
ao = random.choice(an_list)
ap = f'string {ao}'
aq = ap + '8'
ar = aq + '5'
print(ar)