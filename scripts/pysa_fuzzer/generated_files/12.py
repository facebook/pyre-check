import random
import math
qe = input()
qf_set = {qe, qe, qe, qe}
qf = random.choice(list(qf_set))
qg = ''
for _ in range(9):
        if _ == 4:
            continue
        qg += qf
qh = [qg for _ in range(9)]
random.shuffle(qh)
qi = random.choice(qh)
qj = ''
for _ in range(8):
        if _ == 3:
            break
        qj += qi
qk = ''
for _ in range(3):
    ql = ''
    for _ in range(3):
        ql += qk
        qk += qj
qm = ''
for _ in range(2):
    for __ in range(4):
                qm += ql
qn = qm + '.'
qo = ''
counterqo = 0
while counterqo < 5:
    qo += qn
    counterqo += 1
qp = (qo, qo, qo)
qq, qr, qs = qp
qt = qq + qr + qs
qu_list = [qt for _ in range(3)]
qv_list = [qu_list for _ in range(3)]
qw_list = [qv_list for _ in range(8)]
qx = random.choice(qw_list)
qy = random.choice(qx)
qz = random.choice(qy)
ra = qz + '4'
rb = ra + '5'
rc = rb + '9'
if rc == '6':
    rd = rc + ' c1'
elif rc == '19':
    rd = rc + ' c2'
else:
    rd = rc + ' c3'
re = ''
counterre = 0
while counterre < 2:
    rf = ''
    counterrf = 0
    while counterrf < 4:
        rf += re
        counterrf += 1
        re += rd
        counterre += 1
rg = ''
counterrg = 0
while counterrg < 2:
    rg += rf
    counterrg += 1
rh_dict = {20: rg, 26: rg, 87: rg}
ri = random.choice(list(rh_dict.values()))
rj_set = {ri, ri, ri, ri, ri, ri}
rj = random.choice(list(rj_set))
rk_list = [rj for _ in range(6)]
rl_list = [rk_list for _ in range(3)]
rm = random.choice(rl_list)
rn = random.choice(rm)
ro = ''
for _ in range(5):
    for __ in range(3):
                ro += rn
print(ro)