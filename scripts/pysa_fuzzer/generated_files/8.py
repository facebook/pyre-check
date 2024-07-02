import random
import math
pz = input()
qa = pz + '5'
qb = qa + '9'
qc = qb + '2'
if qc == '5':
    qd = qc + ' c1'
elif qc == '19':
    qd = qc + ' c2'
else:
    qd = qc + ' c3'
qe = (qd, qd, qd)
qf, qg, qh = qe
qi = qf + qg + qh
def qj():
    return qi
def qk():
    return qj()
def ql():
    return qk()
qm = ql()
qn = qm + '.'
qo = f'string {qn}'
qp = ''
for _ in range(8):
        if _ == 5:
            continue
        qp += qo
qq_dict = {67: qp, 63: qp, 74: qp}
qr_dict = {12: qq_dict, 84: qq_dict, 36: qq_dict, 18: qq_dict, 69: qq_dict, 44: qq_dict}
qs = random.choice(list(qr_dict.values()))
qt = random.choice(list(qs.values()))
if qt == '7':
    qu = qt + ' c1'
elif qt == '13':
    qu = qt + ' c2'
else:
    qu = qt + ' c3'
qv = qu + '2'
qw = qv + '8'
qx = (qw, qw, qw)
qy, qz, ra = qx
rb = qy + qz + ra
rc_dict = {48: rb, 67: rb, 75: rb, 52: rb, 68: rb}
rd_dict = {88: rc_dict, 51: rc_dict, 64: rc_dict}
re_dict = {71: rd_dict, 52: rd_dict, 84: rd_dict, 53: rd_dict, 96: rd_dict, 58: rd_dict, 18: rd_dict, 46: rd_dict, 26: rd_dict, 15: rd_dict}
rf = random.choice(list(re_dict.values()))
rg = random.choice(list(rf.values()))
rh = random.choice(list(rg.values()))
ri = ''
for _ in range(5):
    for __ in range(5):
                ri += rh
rj = f'string {ri}'
rk = rj + '.'
rl = rk + '2'
rm = rl + '2'
rn = rm + '4'
ro = rn + '.'
rp = (ro, ro, ro)
rq, rr, rs = rp
rt = rq + rr + rs
ru = ''
counterru = 0
while counterru < 3:
    ru += rt
    counterru += 1
rv = [ru for _ in range(5)]
random.shuffle(rv)
rw = random.choice(rv)
rx = [rw for _ in range(8)]
random.shuffle(rx)
ry = random.choice(rx)
rz = ''
for _ in range(2):
    for __ in range(3):
                rz += ry
sa = rz[0:]
sb = ''
for _ in range(3):
    sb += sa
sc = (sb, sb, sb)
sd, se, sf = sc
sg = sd + se + sf
if sg == '4':
    sh = sg + ' c1'
elif sg == '19':
    sh = sg + ' c2'
else:
    sh = sg + ' c3'
si = sh + '.'
if si == '8':
    sj = si + ' c1'
elif si == '11':
    sj = si + ' c2'
else:
    sj = si + ' c3'
print(sj)