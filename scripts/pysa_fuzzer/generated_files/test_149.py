import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = (d, d, d)
f, g, h = e
i = f + g + h
j = i + '.'
k = ''
for _ in range(8):
        if _ == 5:
            break
        k += j
l = k + '.'
m = l[0:]
n = f'string {m}'
o = ''
countero = 0
while countero < 5:
    o += n
    countero += 1
p = o[0:]
q_dict = {57: p, 66: p, 48: p, 12: p, 9: p, 80: p, 85: p, 15: p}
r = random.choice(list(q_dict.values()))
def s():
    return r
def t():
    return s()
def u():
    return t()
v = u()
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = f'string {aa}'
ac_list = [ab for _ in range(8)]
ad_list = [ac_list for _ in range(8)]
ae_list = [ad_list for _ in range(7)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = random.choice(ag)
ai_dict = {9: ah, 70: ah, 43: ah, 19: ah, 31: ah, 65: ah}
aj = random.choice(list(ai_dict.values()))
ak = aj + '.'
al = ''
counteral = 0
while counteral < 3:
    am = ''
    counteram = 0
    while counteram < 5:
        am += al
        counteram += 1
        al += ak
        counteral += 1
an = am[0:]
print(an)