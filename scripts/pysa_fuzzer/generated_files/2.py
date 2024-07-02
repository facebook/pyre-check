import random
import math
be = input()
bf = ''
counterbf = 0
while counterbf < 3:
    bf += be
    counterbf += 1
bg = ''
for _ in range(3):
    bg += bf
bh = ''
for _ in range(7):
        if _ == 5:
            break
        bh += bg
bi = ''
for _ in range(2):
    for __ in range(2):
                bi += bh
bj = [bi for _ in range(5)]
random.shuffle(bj)
bk = random.choice(bj)
bl_set = {bk, bk, bk, bk, bk, bk, bk, bk}
bl = random.choice(list(bl_set))
bm = (bl, bl, bl)
bn, bo, bp = bm
bq = bn + bo + bp
def br():
    return bq
def bs():
    return br()
bt = bs()
bu_set = {bt, bt, bt, bt, bt, bt, bt, bt, bt}
bu = random.choice(list(bu_set))
bv = f'string {bu}'
bw = ''
for _ in range(2):
    bw += bv
bx = bw[0:]
def by():
    return bx
def bz():
    return by()
def ca():
    return bz()
cb = ca()
if cb == '6':
    cc = cb + ' c1'
elif cb == '15':
    cc = cb + ' c2'
else:
    cc = cb + ' c3'
cd = cc[0:]
ce = ''
counterce = 0
while counterce < 5:
    cf = ''
    countercf = 0
    while countercf < 3:
        cg = ''
        countercg = 0
        while countercg < 4:
            cg += cf
            countercg += 1
            cf += ce
            countercf += 1
        ce += cd
        counterce += 1
ch = cg[0:]
if ch == '7':
    ci = ch + ' c1'
elif ch == '17':
    ci = ch + ' c2'
else:
    ci = ch + ' c3'
cj = (ci, ci, ci)
ck, cl, cm = cj
cn = ck + cl + cm
def co():
    return cn
def cp():
    return co()
def cq():
    return cp()
cr = cq()
if cr == '5':
    cs = cr + ' c1'
elif cr == '14':
    cs = cr + ' c2'
else:
    cs = cr + ' c3'
ct = cs + '8'
cu = ct + '3'
cv = cu + '4'
cw = ''
countercw = 0
while countercw < 3:
    cw += cv
    countercw += 1
def cx():
    return cw
def cy():
    return cx()
cz = cy()
da = cz + '.'
db = ''
for _ in range(10):
        if _ == 1:
            break
        db += da
dc = db[0:]
dd = dc + '.'
print(dd)