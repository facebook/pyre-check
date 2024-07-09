import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '20':
    d = b + 'c2'
else:
    d = c + 'c3'
e = d[0:]
f = ''
counterf = 0
while counterf < 4:
    g = ''
    counterg = 0
    while counterg < 2:
        h = ''
        counterh = 0
        while counterh < 5:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i_list = [h for _ in range(8)]
j_list = [i_list for _ in range(6)]
k = random.choice(j_list)
l = random.choice(k)
def m():
    return l
n = m()
o = (n, n, n)
p, q, r = o
s = p + q + r
t = s[0:]
u_dict = {37: t, 97: t, 27: t, 10: t, 35: t, 34: t, 98: t, 2: t, 51: t, 30: t}
v_dict = {22: u_dict, 10: u_dict, 57: u_dict, 94: u_dict, 98: u_dict, 16: u_dict, 96: u_dict, 26: u_dict, 56: u_dict}
w_dict = {12: v_dict, 19: v_dict, 12: v_dict, 60: v_dict, 60: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = random.choice(list(y.values()))
aa = z + '.'
ab = ''
counterab = 0
while counterab < 5:
    ac = ''
    counterac = 0
    while counterac < 5:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
ad = ac + '4'
ae_list = [ad for _ in range(4)]
af_list = [ae_list for _ in range(8)]
ag = random.choice(af_list)
ah = random.choice(ag)
if ah == ah:
    ak = ah + 'c1'
elif ah == '19':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
def al():
    return ak
am = al()
an = ''
for _ in range(4):
    for __ in range(5):
                an += am
def ao():
    return an
ap = ao()
aq = f'string {ap}'
ar = ''
for _ in range(4):
    at = ''
    for _ in range(3):
        at += ar
        ar += aq
print(at)