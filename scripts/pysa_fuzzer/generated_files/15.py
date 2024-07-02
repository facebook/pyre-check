import random
import math
up = input()
uq = [up for _ in range(10)]
random.shuffle(uq)
ur = random.choice(uq)
us_dict = {62: ur, 70: ur, 80: ur, 82: ur, 58: ur, 94: ur}
ut_dict = {5: us_dict, 5: us_dict, 47: us_dict, 6: us_dict, 50: us_dict, 98: us_dict, 87: us_dict, 7: us_dict, 36: us_dict}
uu_dict = {89: ut_dict, 18: ut_dict, 86: ut_dict, 82: ut_dict, 70: ut_dict, 65: ut_dict}
uv = random.choice(list(uu_dict.values()))
uw = random.choice(list(uv.values()))
ux = random.choice(list(uw.values()))
uy_list = [ux for _ in range(9)]
uz = random.choice(uy_list)
va = ''
for _ in range(2):
    for __ in range(5):
                va += uz
def vb():
    return va
vc = vb()
vd = vc + '3'
ve = vd + '3'
vf = ve + '1'
vg_set = {vf, vf, vf, vf, vf, vf, vf}
vg = random.choice(list(vg_set))
vh = [vg for _ in range(6)]
random.shuffle(vh)
vi = random.choice(vh)
vj = ''
for _ in range(3):
    vk = ''
    for _ in range(4):
        vk += vj
        vj += vi
vl = [vk for _ in range(9)]
random.shuffle(vl)
vm = random.choice(vl)
vn = vm + '.'
vo = (vn, vn, vn)
vp, vq, vr = vo
vs = vp + vq + vr
if vs == '2':
    vt = vs + ' c1'
elif vs == '12':
    vt = vs + ' c2'
else:
    vt = vs + ' c3'
if vt == '6':
    vu = vt + ' c1'
elif vt == '19':
    vu = vt + ' c2'
else:
    vu = vt + ' c3'
vv = ''
for _ in range(2):
    vw = ''
    for _ in range(3):
        vx = ''
        for _ in range(3):
            vx += vw
            vw += vv
        vv += vu
vy = (vx, vx, vx)
vz, wa, wb = vy
wc = vz + wa + wb
wd = ''
for _ in range(3):
    for __ in range(2):
                wd += wc
we = [wd for _ in range(5)]
random.shuffle(we)
wf = random.choice(we)
print(wf)