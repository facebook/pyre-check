import random
import math
de = input()
df = ''
counterdf = 0
while counterdf < 4:
    dg = ''
    counterdg = 0
    while counterdg < 2:
        dh = ''
        counterdh = 0
        while counterdh < 3:
            dh += dg
            counterdh += 1
            dg += df
            counterdg += 1
        df += de
        counterdf += 1
di = ''
counterdi = 0
while counterdi < 2:
    dj = ''
    counterdj = 0
    while counterdj < 3:
        dk = ''
        counterdk = 0
        while counterdk < 3:
            dk += dj
            counterdk += 1
            dj += di
            counterdj += 1
        di += dh
        counterdi += 1
dl_list = [dk for _ in range(4)]
dm = random.choice(dl_list)
dn = dm + '9'
do = dn + '3'
dp = do + '7'
dq = (dp, dp, dp)
dr, ds, dt = dq
du = dr + ds + dt
dv = [du for _ in range(6)]
random.shuffle(dv)
dw = random.choice(dv)
dx = [dw for _ in range(5)]
random.shuffle(dx)
dy = random.choice(dx)
def dz():
    return dy
def ea():
    return dz()
def eb():
    return ea()
ec = eb()
ed = ''
for _ in range(4):
    ee = ''
    for _ in range(3):
        ee += ed
        ed += ec
ef = ''
for _ in range(9):
        if _ == 4:
            continue
        ef += ee
eg = (ef, ef, ef)
eh, ei, ej = eg
ek = eh + ei + ej
el_list = [ek for _ in range(7)]
em_list = [el_list for _ in range(5)]
en_list = [em_list for _ in range(9)]
eo = random.choice(en_list)
ep = random.choice(eo)
eq = random.choice(ep)
er_list = [eq for _ in range(6)]
es_list = [er_list for _ in range(5)]
et_list = [es_list for _ in range(7)]
eu = random.choice(et_list)
ev = random.choice(eu)
ew = random.choice(ev)
ex = ''
for _ in range(8):
        if _ == 1:
            break
        ex += ew
ey = f'string {ex}'
if ey == '10':
    ez = ey + ' c1'
elif ey == '11':
    ez = ey + ' c2'
else:
    ez = ey + ' c3'
fa = ez + '.'
fb_dict = {65: fa, 32: fa, 79: fa, 79: fa, 84: fa, 52: fa, 78: fa, 63: fa, 26: fa, 100: fa}
fc = random.choice(list(fb_dict.values()))
fd = ''
for _ in range(2):
    for __ in range(5):
                fd += fc
fe = fd[0:]
ff = ''
for _ in range(4):
    fg = ''
    for _ in range(3):
        fh = ''
        for _ in range(2):
            fh += fg
            fg += ff
        ff += fe
if fh == '9':
    fi = fh + ' c1'
elif fh == '11':
    fi = fh + ' c2'
else:
    fi = fh + ' c3'
fj = f'string {fi}'
fk = (fj, fj, fj)
fl, fm, fn = fk
fo = fl + fm + fn
fp = fo[0:]
fq = fp + '.'
if fq == '1':
    fr = fq + ' c1'
elif fq == '20':
    fr = fq + ' c2'
else:
    fr = fq + ' c3'
def fs():
    return fr
def ft():
    return fs()
def fu():
    return ft()
fv = fu()
print(fv)