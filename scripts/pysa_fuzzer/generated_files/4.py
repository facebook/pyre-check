import random
import math
df = input()
dg = ''
for _ in range(4):
    for __ in range(5):
                dg += df
dh = f'string {dg}'
di_list = [dh for _ in range(5)]
dj_list = [di_list for _ in range(2)]
dk_list = [dj_list for _ in range(5)]
dl = random.choice(dk_list)
dm = random.choice(dl)
dn = random.choice(dm)
if dn == '2':
    do = dn + ' c1'
elif dn == '11':
    do = dn + ' c2'
else:
    do = dn + ' c3'
dp = (do, do, do)
dq, dr, ds = dp
dt = dq + dr + ds
du_list = [dt for _ in range(2)]
dv_list = [du_list for _ in range(2)]
dw_list = [dv_list for _ in range(4)]
dx = random.choice(dw_list)
dy = random.choice(dx)
dz = random.choice(dy)
ea = (dz, dz, dz)
eb, ec, ed = ea
ee = eb + ec + ed
ef = ''
for _ in range(2):
    for __ in range(5):
                ef += ee
eg_list = [ef for _ in range(4)]
eh_list = [eg_list for _ in range(8)]
ei_list = [eh_list for _ in range(2)]
ej = random.choice(ei_list)
ek = random.choice(ej)
el = random.choice(ek)
em = f'string {el}'
en_list = [em for _ in range(7)]
eo_list = [en_list for _ in range(6)]
ep_list = [eo_list for _ in range(5)]
eq = random.choice(ep_list)
er = random.choice(eq)
es = random.choice(er)
et = [es for _ in range(5)]
random.shuffle(et)
eu = random.choice(et)
def ev():
    return eu
def ew():
    return ev()
def ex():
    return ew()
ey = ex()
ez = ''
for _ in range(7):
        if _ == 1:
            continue
        ez += ey
fa_dict = {73: ez, 26: ez, 15: ez, 28: ez, 62: ez, 60: ez, 58: ez, 92: ez}
fb_dict = {45: fa_dict, 39: fa_dict, 75: fa_dict, 3: fa_dict, 65: fa_dict, 4: fa_dict, 94: fa_dict, 9: fa_dict, 33: fa_dict, 22: fa_dict}
fc_dict = {74: fb_dict, 69: fb_dict, 58: fb_dict, 51: fb_dict}
fd = random.choice(list(fc_dict.values()))
fe = random.choice(list(fd.values()))
ff = random.choice(list(fe.values()))
fg = ff[0:]
fh_set = {fg, fg, fg, fg, fg}
fh = random.choice(list(fh_set))
fi = ''
counterfi = 0
while counterfi < 2:
    fi += fh
    counterfi += 1
print(fi)