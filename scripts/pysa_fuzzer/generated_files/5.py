import random
import math
iq = input()
ir = (iq, iq, iq)
it, iu, iv = ir
iw = it + iu + iv
ix = iw + '4'
iy = ix + '4'
iz = iy + '7'
ja = (iz, iz, iz)
jb, jc, jd = ja
je = jb + jc + jd
jf = ''
counterjf = 0
while counterjf < 2:
    jf += je
    counterjf += 1
jg_dict = {44: jf, 99: jf, 3: jf, 94: jf, 16: jf, 40: jf, 64: jf, 42: jf}
jh_dict = {25: jg_dict, 19: jg_dict, 76: jg_dict, 12: jg_dict, 29: jg_dict}
ji_dict = {40: jh_dict, 17: jh_dict, 85: jh_dict, 42: jh_dict}
jj = random.choice(list(ji_dict.values()))
jk = random.choice(list(jj.values()))
jl = random.choice(list(jk.values()))
jm = [jl for _ in range(7)]
random.shuffle(jm)
jn = random.choice(jm)
jo_dict = {59: jn, 63: jn, 19: jn, 12: jn, 87: jn, 72: jn, 49: jn, 6: jn, 97: jn}
jp_dict = {86: jo_dict, 74: jo_dict, 19: jo_dict, 57: jo_dict, 71: jo_dict, 13: jo_dict, 42: jo_dict}
jq_dict = {72: jp_dict, 75: jp_dict, 22: jp_dict, 95: jp_dict, 33: jp_dict}
jr = random.choice(list(jq_dict.values()))
js = random.choice(list(jr.values()))
jt = random.choice(list(js.values()))
ju = ''
for _ in range(5):
    jv = ''
    for _ in range(4):
        jv += ju
        ju += jt
jw = jv[0:]
jx = jw[0:]
jy = jx + '.'
jz = ''
for _ in range(5):
    for __ in range(2):
                jz += jy
ka = jz[0:]
kb = ka + '.'
kc_list = [kb for _ in range(5)]
kd_list = [kc_list for _ in range(4)]
ke = random.choice(kd_list)
kf = random.choice(ke)
kg_dict = {39: kf, 20: kf, 44: kf, 70: kf, 53: kf, 73: kf, 55: kf}
kh_dict = {46: kg_dict, 62: kg_dict, 92: kg_dict, 53: kg_dict, 92: kg_dict, 76: kg_dict, 70: kg_dict}
ki_dict = {11: kh_dict, 9: kh_dict, 46: kh_dict, 18: kh_dict, 59: kh_dict, 5: kh_dict, 6: kh_dict, 28: kh_dict, 23: kh_dict}
kj = random.choice(list(ki_dict.values()))
kk = random.choice(list(kj.values()))
kl = random.choice(list(kk.values()))
km = ''
counterkm = 0
while counterkm < 3:
    kn = ''
    counterkn = 0
    while counterkn < 3:
        kn += km
        counterkn += 1
        km += kl
        counterkm += 1
ko = kn + '.'
def kp():
    return ko
def kq():
    return kp()
kr = kq()
ks = ''
for _ in range(5):
        if _ == 1:
            continue
        ks += kr
kt = ''
for _ in range(8):
        if _ == 1:
            break
        kt += ks
ku_list = [kt for _ in range(7)]
kv_list = [ku_list for _ in range(5)]
kw = random.choice(kv_list)
kx = random.choice(kw)
if kx == '8':
    ky = kx + ' c1'
elif kx == '12':
    ky = kx + ' c2'
else:
    ky = kx + ' c3'
def kz():
    return ky
def la():
    return kz()
def lb():
    return la()
lc = lb()
ld = lc[0:]
le = ld + '8'
lf_list = [le for _ in range(6)]
lg = random.choice(lf_list)
lh = lg[0:]
print(lh)