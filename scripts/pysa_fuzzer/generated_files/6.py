import random
import math
li = input()
lj = ''
for _ in range(2):
    for __ in range(3):
                lj += li
lk = lj[0:]
if lk == '7':
    ll = lk + ' c1'
elif lk == '16':
    ll = lk + ' c2'
else:
    ll = lk + ' c3'
lm = [ll for _ in range(7)]
random.shuffle(lm)
ln = random.choice(lm)
lo = (ln, ln, ln)
lp, lq, lr = lo
ls = lp + lq + lr
lt_set = {ls, ls, ls, ls, ls, ls}
lt = random.choice(list(lt_set))
lu_dict = {21: lt, 7: lt, 53: lt, 42: lt, 16: lt, 60: lt, 17: lt}
lv_dict = {98: lu_dict, 92: lu_dict, 100: lu_dict, 30: lu_dict, 68: lu_dict, 57: lu_dict, 2: lu_dict, 100: lu_dict, 32: lu_dict, 50: lu_dict}
lw = random.choice(list(lv_dict.values()))
lx = random.choice(list(lw.values()))
ly = ''
counterly = 0
while counterly < 3:
    lz = ''
    counterlz = 0
    while counterlz < 3:
        ma = ''
        counterma = 0
        while counterma < 4:
            ma += lz
            counterma += 1
            lz += ly
            counterlz += 1
        ly += lx
        counterly += 1
mb = [ma for _ in range(6)]
random.shuffle(mb)
mc = random.choice(mb)
md = ''
for _ in range(3):
    md += mc
def me():
    return md
def mf():
    return me()
mg = mf()
mh_dict = {49: mg, 13: mg}
mi_dict = {63: mh_dict, 89: mh_dict, 47: mh_dict, 38: mh_dict}
mj = random.choice(list(mi_dict.values()))
mk = random.choice(list(mj.values()))
ml = ''
for _ in range(3):
    for __ in range(4):
                ml += mk
mm = ''
for _ in range(5):
    mn = ''
    for _ in range(3):
        mn += mm
        mm += ml
mo = ''
for _ in range(5):
        if _ == 5:
            break
        mo += mn
mp = mo + '4'
mq = mp + '6'
mr = mq + '5'
def ms():
    return mr
def mt():
    return ms()
mu = mt()
mv = ''
for _ in range(7):
        if _ == 3:
            continue
        mv += mu
mw = (mv, mv, mv)
mx, my, mz = mw
na = mx + my + mz
if na == '2':
    nb = na + ' c1'
elif na == '15':
    nb = na + ' c2'
else:
    nb = na + ' c3'
nc = nb + '9'
nd = nc + '4'
ne = nd + '3'
nf = ''
for _ in range(2):
    ng = ''
    for _ in range(2):
        nh = ''
        for _ in range(2):
            nh += ng
            ng += nf
        nf += ne
ni = nh[0:]
nj = ''
for _ in range(3):
    for __ in range(2):
                nj += ni
nk = nj + '2'
nl = nk + '3'
nm = nl + '6'
nn = nm[0:]
no_list = [nn for _ in range(3)]
np_list = [no_list for _ in range(8)]
nq_list = [np_list for _ in range(5)]
nr = random.choice(nq_list)
ns = random.choice(nr)
nt = random.choice(ns)
if nt == '9':
    nu = nt + ' c1'
elif nt == '19':
    nu = nt + ' c2'
else:
    nu = nt + ' c3'
print(nu)