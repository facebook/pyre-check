import random
import math

a = input()
b = a + '.'
c = ''
for _ in range(8):
        if _ == 1:
            continue
        c += b
d = ''
for _ in range(5):
        if _ == 5:
            break
        d += c
e = ''
for _ in range(5):
    e += d
f = ''
counterf = 0
while counterf < 3:
    g = ''
    counterg = 0
    while counterg < 3:
        g += f
        counterg += 1
        f += e
        counterf += 1
if g == g:
    j = g + 'c1'
elif g == '17':
    j = h + 'c2'
else:
    j = i + 'c3'
k = (j, j, j)
l, m, n = k
o = l + m + n
p = (o, o, o)
q, r, s = p
t = q + r + s
u = (t, t, t)
v, w, x = u
y = v + w + x
z_dict = {31: y, 37: y, 84: y, 98: y, 61: y, 61: y, 76: y, 26: y}
aa_dict = {46: z_dict, 8: z_dict, 44: z_dict, 78: z_dict, 5: z_dict, 53: z_dict, 3: z_dict, 26: z_dict, 78: z_dict, 42: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = ac + '6'
ae_dict = {73: ad, 29: ad, 82: ad, 1: ad, 28: ad, 81: ad, 94: ad, 96: ad, 44: ad}
af_dict = {6: ae_dict, 59: ae_dict, 52: ae_dict, 86: ae_dict, 14: ae_dict, 26: ae_dict, 9: ae_dict, 51: ae_dict, 4: ae_dict, 72: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai_dict = {63: ah, 65: ah, 31: ah, 38: ah}
aj_dict = {25: ai_dict, 38: ai_dict, 18: ai_dict, 20: ai_dict, 52: ai_dict, 83: ai_dict, 49: ai_dict, 52: ai_dict, 89: ai_dict, 99: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = ''
for _ in range(2):
    for __ in range(3):
                am += al
an = am[0:]
ao = ''
for _ in range(4):
    for __ in range(2):
                ao += an
ap = ''
for _ in range(2):
    for __ in range(4):
                ap += ao
aq = ''
for _ in range(10):
        if _ == 5:
            break
        aq += ap
print(aq)