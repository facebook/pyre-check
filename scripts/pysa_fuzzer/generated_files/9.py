import random
import math
sk = input()
sl = [sk for _ in range(8)]
random.shuffle(sl)
sm = random.choice(sl)
sn = sm + '1'
so = sn + '4'
sp = so + '5'
sq = ''
countersq = 0
while countersq < 3:
    sr = ''
    countersr = 0
    while countersr < 5:
        ss = ''
        counterss = 0
        while counterss < 2:
            ss += sr
            counterss += 1
            sr += sq
            countersr += 1
        sq += sp
        countersq += 1
st = ''
for _ in range(5):
        if _ == 5:
            break
        st += ss
def su():
    return st
def sv():
    return su()
def sw():
    return sv()
sx = sw()
sy_dict = {15: sx, 81: sx, 93: sx, 87: sx, 31: sx, 86: sx}
sz_dict = {76: sy_dict, 99: sy_dict, 30: sy_dict, 43: sy_dict, 10: sy_dict}
ta = random.choice(list(sz_dict.values()))
tb = random.choice(list(ta.values()))
tc = tb + '.'
td = tc + '.'
te_dict = {18: td, 80: td, 89: td, 6: td, 51: td}
tf = random.choice(list(te_dict.values()))
tg = tf[0:]
th = ''
for _ in range(2):
    for __ in range(4):
                th += tg
ti = ''
for _ in range(4):
    tj = ''
    for _ in range(5):
        tj += ti
        ti += th
tk = f'string {tj}'
tl = tk + '.'
tm = ''
for _ in range(3):
    tn = ''
    for _ in range(4):
        tn += tm
        tm += tl
if tn == '7':
    to = tn + ' c1'
elif tn == '17':
    to = tn + ' c2'
else:
    to = tn + ' c3'
tp_dict = {49: to, 27: to, 20: to, 89: to, 50: to}
tq = random.choice(list(tp_dict.values()))
if tq == '2':
    tr = tq + ' c1'
elif tq == '16':
    tr = tq + ' c2'
else:
    tr = tq + ' c3'
ts = ''
counterts = 0
while counterts < 5:
    tt = ''
    countertt = 0
    while countertt < 4:
        tt += ts
        countertt += 1
        ts += tr
        counterts += 1
tu = tt[0:]
tv = tu + '.'
tw_list = [tv for _ in range(8)]
tx = random.choice(tw_list)
def ty():
    return tx
def tz():
    return ty()
ua = tz()
def ub():
    return ua
def uc():
    return ub()
ud = uc()
ue = f'string {ud}'
uf = ''
for _ in range(3):
    for __ in range(5):
                uf += ue
ug = ''
counterug = 0
while counterug < 5:
    uh = ''
    counteruh = 0
    while counteruh < 5:
        ui = ''
        counterui = 0
        while counterui < 5:
            ui += uh
            counterui += 1
            uh += ug
            counteruh += 1
        ug += uf
        counterug += 1
def uj():
    return ui
def uk():
    return uj()
def ul():
    return uk()
um = ul()
print(um)