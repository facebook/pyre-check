import random
import math
un = input()
uo = ''
for _ in range(5):
        if _ == 4:
            break
        uo += un
up_list = [uo for _ in range(5)]
uq = random.choice(up_list)
ur_dict = {85: uq, 8: uq, 57: uq}
us = random.choice(list(ur_dict.values()))
ut = f'string {us}'
uu = f'string {ut}'
uv_set = {uu, uu, uu, uu, uu, uu}
uv = random.choice(list(uv_set))
uw = ''
for _ in range(5):
        if _ == 3:
            break
        uw += uv
ux = uw + '8'
uy = ux + '8'
uz_set = {uy, uy, uy, uy, uy, uy, uy, uy, uy, uy}
uz = random.choice(list(uz_set))
if uz == '8':
    va = uz + ' c1'
elif uz == '17':
    va = uz + ' c2'
else:
    va = uz + ' c3'
vb = [va for _ in range(7)]
random.shuffle(vb)
vc = random.choice(vb)
vd = vc[0:]
ve_list = [vd for _ in range(7)]
vf_list = [ve_list for _ in range(4)]
vg = random.choice(vf_list)
vh = random.choice(vg)
vi = ''
for _ in range(8):
        if _ == 3:
            break
        vi += vh
vj = vi + '.'
vk = f'string {vj}'
vl = [vk for _ in range(9)]
random.shuffle(vl)
vm = random.choice(vl)
vn = vm + '.'
vo_dict = {50: vn, 61: vn, 78: vn, 77: vn, 48: vn, 26: vn, 81: vn, 85: vn, 31: vn, 58: vn}
vp_dict = {45: vo_dict, 57: vo_dict, 53: vo_dict}
vq_dict = {8: vp_dict, 57: vp_dict, 67: vp_dict, 34: vp_dict, 95: vp_dict, 27: vp_dict, 94: vp_dict, 22: vp_dict, 33: vp_dict}
vr = random.choice(list(vq_dict.values()))
vs = random.choice(list(vr.values()))
vt = random.choice(list(vs.values()))
vu = ''
countervu = 0
while countervu < 3:
    vv = ''
    countervv = 0
    while countervv < 4:
        vw = ''
        countervw = 0
        while countervw < 3:
            vw += vv
            countervw += 1
            vv += vu
            countervv += 1
        vu += vt
        countervu += 1
vx = vw + '3'
vy = vx + '4'
def vz():
    return vy
wa = vz()
wb = ''
counterwb = 0
while counterwb < 5:
    wc = ''
    counterwc = 0
    while counterwc < 5:
        wd = ''
        counterwd = 0
        while counterwd < 2:
            wd += wc
            counterwd += 1
            wc += wb
            counterwc += 1
        wb += wa
        counterwb += 1
we_set = {wd, wd, wd, wd, wd, wd, wd, wd, wd}
we = random.choice(list(we_set))
wf = ''
counterwf = 0
while counterwf < 5:
    wf += we
    counterwf += 1
wg_dict = {20: wf, 93: wf, 31: wf, 63: wf, 97: wf}
wh_dict = {95: wg_dict, 38: wg_dict, 59: wg_dict, 26: wg_dict, 74: wg_dict, 15: wg_dict, 10: wg_dict, 40: wg_dict, 63: wg_dict, 78: wg_dict}
wi_dict = {94: wh_dict, 21: wh_dict, 67: wh_dict, 92: wh_dict, 90: wh_dict, 68: wh_dict, 43: wh_dict, 52: wh_dict, 51: wh_dict}
wj = random.choice(list(wi_dict.values()))
wk = random.choice(list(wj.values()))
wl = random.choice(list(wk.values()))
wm = ''
for _ in range(9):
        if _ == 3:
            continue
        wm += wl
wn = f'string {wm}'
print(wn)