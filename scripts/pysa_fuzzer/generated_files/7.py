import random
import math
iv = input()
iw = ''
for _ in range(5):
    ix = ''
    for _ in range(4):
        iy = ''
        for _ in range(3):
            iy += ix
            ix += iw
        iw += iv
iz = ''
counteriz = 0
while counteriz < 2:
    ja = ''
    counterja = 0
    while counterja < 5:
        ja += iz
        counterja += 1
        iz += iy
        counteriz += 1
jb_set = {ja, ja, ja, ja, ja, ja}
jb = random.choice(list(jb_set))
jc = jb[0:]
if jc == '6':
    jd = jc + ' c1'
elif jc == '16':
    jd = jc + ' c2'
else:
    jd = jc + ' c3'
je = [jd for _ in range(8)]
random.shuffle(je)
jf = random.choice(je)
jg = ''
for _ in range(5):
    jh = ''
    for _ in range(2):
        ji = ''
        for _ in range(5):
            ji += jh
            jh += jg
        jg += jf
jj_dict = {20: ji, 61: ji, 77: ji, 87: ji, 90: ji, 95: ji}
jk_dict = {87: jj_dict, 25: jj_dict, 45: jj_dict, 92: jj_dict, 61: jj_dict, 48: jj_dict, 29: jj_dict, 6: jj_dict, 37: jj_dict, 20: jj_dict}
jl = random.choice(list(jk_dict.values()))
jm = random.choice(list(jl.values()))
jn = f'string {jm}'
jo = (jn, jn, jn)
jp, jq, jr = jo
js = jp + jq + jr
jt = ''
for _ in range(3):
    for __ in range(2):
                jt += js
ju_set = {jt, jt, jt, jt, jt, jt, jt, jt, jt}
ju = random.choice(list(ju_set))
jv = ju + '.'
jw_dict = {36: jv, 83: jv, 36: jv, 63: jv, 28: jv, 53: jv, 51: jv, 9: jv, 98: jv, 2: jv}
jx_dict = {87: jw_dict, 8: jw_dict, 71: jw_dict, 67: jw_dict, 53: jw_dict, 15: jw_dict, 78: jw_dict, 37: jw_dict}
jy_dict = {97: jx_dict, 27: jx_dict}
jz = random.choice(list(jy_dict.values()))
ka = random.choice(list(jz.values()))
kb = random.choice(list(ka.values()))
kc = kb + '8'
kd = kc + '8'
ke = kd + '4'
kf = ''
for _ in range(5):
    kg = ''
    for _ in range(5):
        kg += kf
        kf += ke
kh = ''
for _ in range(5):
    for __ in range(5):
                kh += kg
ki = kh[0:]
print(ki)