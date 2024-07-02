import random
import math
rp = input()
def rq():
    return rp
def rr():
    return rq()
def rs():
    return rr()
rt = rs()
ru = rt[0:]
rv_dict = {49: ru, 58: ru, 41: ru}
rw_dict = {35: rv_dict, 15: rv_dict, 93: rv_dict, 32: rv_dict, 6: rv_dict, 69: rv_dict, 46: rv_dict, 60: rv_dict, 14: rv_dict}
rx_dict = {68: rw_dict, 85: rw_dict, 5: rw_dict}
ry = random.choice(list(rx_dict.values()))
rz = random.choice(list(ry.values()))
sa = random.choice(list(rz.values()))
sb = sa + '4'
sc = sb + '9'
sd = sc + '4'
se = (sd, sd, sd)
sf, sg, sh = se
si = sf + sg + sh
sj = f'string {si}'
sk = f'string {sj}'
sl = [sk for _ in range(8)]
random.shuffle(sl)
sm = random.choice(sl)
sn = f'string {sm}'
so = ''
for _ in range(6):
        if _ == 1:
            break
        so += sn
sp_list = [so for _ in range(7)]
sq_list = [sp_list for _ in range(5)]
sr = random.choice(sq_list)
ss = random.choice(sr)
st = ''
for _ in range(5):
        if _ == 2:
            continue
        st += ss
su_set = {st, st, st, st, st, st, st, st, st}
su = random.choice(list(su_set))
sv = f'string {su}'
sw = sv + '9'
sx_dict = {75: sw, 4: sw, 40: sw, 43: sw, 97: sw, 87: sw}
sy_dict = {75: sx_dict, 65: sx_dict, 5: sx_dict, 29: sx_dict, 81: sx_dict, 30: sx_dict, 23: sx_dict, 33: sx_dict}
sz_dict = {84: sy_dict, 20: sy_dict, 82: sy_dict, 90: sy_dict, 39: sy_dict, 5: sy_dict}
ta = random.choice(list(sz_dict.values()))
tb = random.choice(list(ta.values()))
tc = random.choice(list(tb.values()))
td = f'string {tc}'
def te():
    return td
def tf():
    return te()
tg = tf()
print(tg)