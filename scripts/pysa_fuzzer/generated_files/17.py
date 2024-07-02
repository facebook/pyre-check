import random
import math
yb = input()
yc_dict = {67: yb, 21: yb, 53: yb, 31: yb, 78: yb, 57: yb, 49: yb}
yd_dict = {97: yc_dict, 28: yc_dict, 36: yc_dict, 9: yc_dict}
ye_dict = {37: yd_dict, 59: yd_dict}
yf = random.choice(list(ye_dict.values()))
yg = random.choice(list(yf.values()))
yh = random.choice(list(yg.values()))
yi_set = {yh, yh, yh, yh, yh, yh, yh, yh, yh, yh}
yi = random.choice(list(yi_set))
yj = yi + '.'
yk_set = {yj, yj, yj, yj, yj, yj, yj, yj, yj, yj}
yk = random.choice(list(yk_set))
yl = ''
counteryl = 0
while counteryl < 2:
    yl += yk
    counteryl += 1
ym = yl + '.'
yn_set = {ym, ym, ym, ym, ym, ym}
yn = random.choice(list(yn_set))
yo = yn[0:]
yp_dict = {68: yo, 23: yo, 29: yo, 75: yo, 83: yo}
yq_dict = {94: yp_dict, 17: yp_dict, 43: yp_dict, 66: yp_dict, 23: yp_dict, 57: yp_dict}
yr_dict = {31: yq_dict, 47: yq_dict, 14: yq_dict, 61: yq_dict, 10: yq_dict, 2: yq_dict}
ys = random.choice(list(yr_dict.values()))
yt = random.choice(list(ys.values()))
yu = random.choice(list(yt.values()))
yv = (yu, yu, yu)
yw, yx, yy = yv
yz = yw + yx + yy
if yz == '2':
    za = yz + ' c1'
elif yz == '17':
    za = yz + ' c2'
else:
    za = yz + ' c3'
zb_dict = {93: za, 5: za}
zc_dict = {62: zb_dict, 29: zb_dict, 80: zb_dict, 11: zb_dict, 95: zb_dict, 71: zb_dict, 87: zb_dict, 15: zb_dict, 73: zb_dict}
zd_dict = {91: zc_dict, 62: zc_dict, 1: zc_dict, 18: zc_dict, 68: zc_dict, 69: zc_dict, 22: zc_dict, 21: zc_dict, 71: zc_dict, 75: zc_dict}
ze = random.choice(list(zd_dict.values()))
zf = random.choice(list(ze.values()))
zg = random.choice(list(zf.values()))
zh = ''
counterzh = 0
while counterzh < 5:
    zh += zg
    counterzh += 1
zi = f'string {zh}'
zj = ''
counterzj = 0
while counterzj < 2:
    zk = ''
    counterzk = 0
    while counterzk < 3:
        zl = ''
        counterzl = 0
        while counterzl < 4:
            zl += zk
            counterzl += 1
            zk += zj
            counterzk += 1
        zj += zi
        counterzj += 1
zm = (zl, zl, zl)
zn, zo, zp = zm
zq = zn + zo + zp
zr = zq + '.'
zs_set = {zr, zr, zr, zr, zr, zr, zr, zr, zr}
zs = random.choice(list(zs_set))
print(zs)