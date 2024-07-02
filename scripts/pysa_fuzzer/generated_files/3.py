import random
import math
br = input()
bs_list = [br for _ in range(2)]
bt_list = [bs_list for _ in range(8)]
bu_list = [bt_list for _ in range(3)]
bv = random.choice(bu_list)
bw = random.choice(bv)
bx = random.choice(bw)
by = bx + '.'
bz = by + '7'
ca = bz + '7'
cb = ca + '2'
cc = cb + '3'
cd = cc + '.'
ce = ''
for _ in range(5):
    for __ in range(5):
                ce += cd
cf = ce + '.'
cg_list = [cf for _ in range(10)]
ch_list = [cg_list for _ in range(4)]
ci = random.choice(ch_list)
cj = random.choice(ci)
ck = ''
for _ in range(4):
    for __ in range(2):
                ck += cj
cl = ''
for _ in range(8):
        if _ == 2:
            continue
        cl += ck
cm = (cl, cl, cl)
cn, co, cp = cm
cq = cn + co + cp
cr = ''
for _ in range(3):
    cs = ''
    for _ in range(2):
        cs += cr
        cr += cq
ct = ''
counterct = 0
while counterct < 4:
    ct += cs
    counterct += 1
cu = (ct, ct, ct)
cv, cw, cx = cu
cy = cv + cw + cx
cz = ''
for _ in range(3):
    for __ in range(2):
                cz += cy
da = ''
for _ in range(7):
        if _ == 4:
            break
        da += cz
db_set = {da, da, da, da}
db = random.choice(list(db_set))
dc = ''
counterdc = 0
while counterdc < 3:
    dd = ''
    counterdd = 0
    while counterdd < 4:
        de = ''
        counterde = 0
        while counterde < 4:
            de += dd
            counterde += 1
            dd += dc
            counterdd += 1
        dc += db
        counterdc += 1
print(de)