import random
import math
fw = input()
fx = fw + '.'
fy = (fx, fx, fx)
fz, ga, gb = fy
gc = fz + ga + gb
gd = gc + '.'
def ge():
    return gd
gf = ge()
gg = ''
for _ in range(10):
        if _ == 3:
            continue
        gg += gf
gh_set = {gg, gg, gg, gg, gg, gg, gg, gg, gg}
gh = random.choice(list(gh_set))
gi = ''
for _ in range(6):
        if _ == 1:
            break
        gi += gh
gj_list = [gi for _ in range(9)]
gk_list = [gj_list for _ in range(9)]
gl = random.choice(gk_list)
gm = random.choice(gl)
gn_list = [gm for _ in range(7)]
go_list = [gn_list for _ in range(3)]
gp_list = [go_list for _ in range(6)]
gq = random.choice(gp_list)
gr = random.choice(gq)
gs = random.choice(gr)
gt_dict = {77: gs, 9: gs, 68: gs, 65: gs, 28: gs, 66: gs, 63: gs}
gu_dict = {15: gt_dict, 43: gt_dict, 2: gt_dict, 70: gt_dict}
gv = random.choice(list(gu_dict.values()))
gw = random.choice(list(gv.values()))
gx = ''
for _ in range(9):
        if _ == 2:
            break
        gx += gw
gy = [gx for _ in range(5)]
random.shuffle(gy)
gz = random.choice(gy)
ha_dict = {2: gz, 96: gz}
hb_dict = {2: ha_dict, 61: ha_dict}
hc_dict = {44: hb_dict, 80: hb_dict}
hd = random.choice(list(hc_dict.values()))
he = random.choice(list(hd.values()))
hf = random.choice(list(he.values()))
hg = [hf for _ in range(7)]
random.shuffle(hg)
hh = random.choice(hg)
hi = ''
for _ in range(5):
    for __ in range(4):
                hi += hh
hj = hi + '.'
if hj == '7':
    hk = hj + ' c1'
elif hj == '16':
    hk = hj + ' c2'
else:
    hk = hj + ' c3'
if hk == '2':
    hl = hk + ' c1'
elif hk == '15':
    hl = hk + ' c2'
else:
    hl = hk + ' c3'
hm = ''
for _ in range(2):
    hn = ''
    for _ in range(2):
        hn += hm
        hm += hl
ho = ''
counterho = 0
while counterho < 4:
    hp = ''
    counterhp = 0
    while counterhp < 5:
        hp += ho
        counterhp += 1
        ho += hn
        counterho += 1
hq = [hp for _ in range(6)]
random.shuffle(hq)
hr = random.choice(hq)
hs_list = [hr for _ in range(7)]
ht = random.choice(hs_list)
hu = (ht, ht, ht)
hv, hw, hx = hu
hy = hv + hw + hx
hz = hy + '.'
ia = ''
counteria = 0
while counteria < 2:
    ib = ''
    counterib = 0
    while counterib < 2:
        ic = ''
        counteric = 0
        while counteric < 4:
            ic += ib
            counteric += 1
            ib += ia
            counterib += 1
        ia += hz
        counteria += 1
id_list = [ic for _ in range(7)]
ie_list = [id_list for _ in range(3)]
ig = random.choice(ie_list)
ih = random.choice(ig)
ii_dict = {74: ih, 22: ih, 90: ih, 50: ih, 53: ih, 14: ih, 33: ih, 84: ih}
ij_dict = {20: ii_dict, 40: ii_dict, 43: ii_dict, 79: ii_dict, 91: ii_dict, 73: ii_dict, 15: ii_dict, 7: ii_dict, 87: ii_dict}
ik_dict = {23: ij_dict, 54: ij_dict, 92: ij_dict, 79: ij_dict, 82: ij_dict, 55: ij_dict}
il = random.choice(list(ik_dict.values()))
im = random.choice(list(il.values()))
io = random.choice(list(im.values()))
ip = ''
for _ in range(3):
    for __ in range(4):
                ip += io
print(ip)