import random
import math
a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 5:
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
countere = 0
while countere < 2:
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
        e += d
        countere += 1
h = g + '.'
i = h + '7'
j = i + '6'
k = j + '3'
l = ''
counterl = 0
while counterl < 5:
    m = ''
    counterm = 0
    while counterm < 5:
        n = ''
        countern = 0
        while countern < 3:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = ''
for _ in range(2):
    for __ in range(4):
                o += n
p = (o, o, o)
q, r, s = p
t = q + r + s
u = f'string {t}'
v_dict = {21: u, 78: u, 41: u, 87: u, 78: u, 18: u, 80: u, 11: u, 9: u, 81: u}
w_dict = {76: v_dict, 21: v_dict, 49: v_dict, 4: v_dict, 51: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z_list = [y for _ in range(4)]
aa_list = [z_list for _ in range(7)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad_list = [ac for _ in range(10)]
ae_list = [ad_list for _ in range(5)]
af = random.choice(ae_list)
ag = random.choice(af)
def ah():
    return ag
ai = ah()
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = an + '.'
ap = ''
counterap = 0
while counterap < 2:
    aq = ''
    counteraq = 0
    while counteraq < 2:
        ar = ''
        counterar = 0
        while counterar < 3:
            ar += aq
            counterar += 1
            aq += ap
            counteraq += 1
        ap += ao
        counterap += 1
at = ar + '5'
au = at + '9'
av = au + '3'
def aw():
    return av
ax = aw()
ay_list = [ax for _ in range(4)]
az_list = [ay_list for _ in range(6)]
ba_list = [az_list for _ in range(4)]
bb = random.choice(ba_list)
bc = random.choice(bb)
bd = random.choice(bc)
print(bd)