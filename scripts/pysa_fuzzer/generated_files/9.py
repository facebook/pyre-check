import random
import math
ll = input()
lm = f'string {ll}'
ln = lm + '.'
lo_list = [ln for _ in range(6)]
lp_list = [lo_list for _ in range(4)]
lq_list = [lp_list for _ in range(2)]
lr = random.choice(lq_list)
ls = random.choice(lr)
lt = random.choice(ls)
lu_set = {lt, lt, lt, lt, lt}
lu = random.choice(list(lu_set))
lv_dict = {66: lu, 92: lu, 58: lu, 46: lu}
lw_dict = {98: lv_dict, 53: lv_dict, 88: lv_dict, 44: lv_dict, 64: lv_dict}
lx_dict = {59: lw_dict, 18: lw_dict, 41: lw_dict, 85: lw_dict, 9: lw_dict, 89: lw_dict, 98: lw_dict}
ly = random.choice(list(lx_dict.values()))
lz = random.choice(list(ly.values()))
ma = random.choice(list(lz.values()))
mb = ''
for _ in range(10):
        if _ == 3:
            continue
        mb += ma
mc = ''
countermc = 0
while countermc < 2:
    md = ''
    countermd = 0
    while countermd < 5:
        me = ''
        counterme = 0
        while counterme < 3:
            me += md
            counterme += 1
            md += mc
            countermd += 1
        mc += mb
        countermc += 1
mf = ''
countermf = 0
while countermf < 5:
    mg = ''
    countermg = 0
    while countermg < 2:
        mh = ''
        countermh = 0
        while countermh < 5:
            mh += mg
            countermh += 1
            mg += mf
            countermg += 1
        mf += me
        countermf += 1
mi = ''
for _ in range(3):
    mj = ''
    for _ in range(5):
        mk = ''
        for _ in range(5):
            mk += mj
            mj += mi
        mi += mh
ml = ''
for _ in range(3):
    ml += mk
mm = [ml for _ in range(6)]
random.shuffle(mm)
mn = random.choice(mm)
mo = mn + '.'
mp_dict = {86: mo, 5: mo, 7: mo, 32: mo, 93: mo, 52: mo}
mq_dict = {58: mp_dict, 1: mp_dict, 16: mp_dict, 42: mp_dict, 63: mp_dict, 13: mp_dict, 85: mp_dict, 25: mp_dict, 20: mp_dict}
mr = random.choice(list(mq_dict.values()))
ms = random.choice(list(mr.values()))
mt = (ms, ms, ms)
mu, mv, mw = mt
mx = mu + mv + mw
my = ''
for _ in range(4):
    mz = ''
    for _ in range(4):
        na = ''
        for _ in range(4):
            na += mz
            mz += my
        my += mx
nb = na + '.'
nc = nb + '2'
nd = [nc for _ in range(5)]
random.shuffle(nd)
ne = random.choice(nd)
print(ne)