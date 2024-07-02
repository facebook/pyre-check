import random
import math
wg = input()
wh_list = [wg for _ in range(2)]
wi_list = [wh_list for _ in range(5)]
wj = random.choice(wi_list)
wk = random.choice(wj)
wl_list = [wk for _ in range(5)]
wm_list = [wl_list for _ in range(5)]
wn_list = [wm_list for _ in range(5)]
wo = random.choice(wn_list)
wp = random.choice(wo)
wq = random.choice(wp)
wr = wq + '.'
ws = (wr, wr, wr)
wt, wu, wv = ws
ww = wt + wu + wv
wx = ''
for _ in range(2):
    for __ in range(4):
                wx += ww
wy = ''
counterwy = 0
while counterwy < 5:
    wz = ''
    counterwz = 0
    while counterwz < 4:
        wz += wy
        counterwz += 1
        wy += wx
        counterwy += 1
xa = wz[0:]
xb = (xa, xa, xa)
xc, xd, xe = xb
xf = xc + xd + xe
xg = ''
for _ in range(4):
    xh = ''
    for _ in range(2):
        xi = ''
        for _ in range(3):
            xi += xh
            xh += xg
        xg += xf
xj = f'string {xi}'
xk = xj[0:]
if xk == '7':
    xl = xk + ' c1'
elif xk == '12':
    xl = xk + ' c2'
else:
    xl = xk + ' c3'
xm_list = [xl for _ in range(4)]
xn = random.choice(xm_list)
xo = xn + '3'
xp = xo + '6'
xq = xp + '2'
xr = xq[0:]
def xs():
    return xr
def xt():
    return xs()
xu = xt()
xv = ''
counterxv = 0
while counterxv < 3:
    xw = ''
    counterxw = 0
    while counterxw < 4:
        xw += xv
        counterxw += 1
        xv += xu
        counterxv += 1
xx_dict = {13: xw, 71: xw, 67: xw, 79: xw, 30: xw, 32: xw}
xy_dict = {17: xx_dict, 70: xx_dict, 29: xx_dict, 65: xx_dict, 44: xx_dict}
xz = random.choice(list(xy_dict.values()))
ya = random.choice(list(xz.values()))
print(ya)