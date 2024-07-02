import random
import math
ab = input()
ac = f'string {ab}'
ad_dict = {3: ac, 49: ac, 35: ac}
ae_dict = {3: ad_dict, 16: ad_dict, 62: ad_dict, 40: ad_dict, 78: ad_dict, 67: ad_dict, 39: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = ''
counterah = 0
while counterah < 4:
    ai = ''
    counterai = 0
    while counterai < 5:
        ai += ah
        counterai += 1
        ah += ag
        counterah += 1
aj_set = {ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
if aj == '2':
    ak = aj + ' c1'
elif aj == '13':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
al = ''
for _ in range(5):
    for __ in range(4):
                al += ak
am = ''
counteram = 0
while counteram < 2:
    an = ''
    counteran = 0
    while counteran < 4:
        ao = ''
        counterao = 0
        while counterao < 3:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
av = ''
for _ in range(6):
        if _ == 4:
            break
        av += au
aw = ''
counteraw = 0
while counteraw < 3:
    ax = ''
    counterax = 0
    while counterax < 3:
        ay = ''
        counteray = 0
        while counteray < 2:
            ay += ax
            counteray += 1
            ax += aw
            counterax += 1
        aw += av
        counteraw += 1
az = (ay, ay, ay)
ba, bb, bc = az
bd = ba + bb + bc
be = f'string {bd}'
bf = [be for _ in range(8)]
random.shuffle(bf)
bg = random.choice(bf)
bh_set = {bg, bg, bg, bg, bg, bg, bg}
bh = random.choice(list(bh_set))
bi = (bh, bh, bh)
bj, bk, bl = bi
bm = bj + bk + bl
def bn():
    return bm
bo = bn()
bp = bo[0:]
bq = bp[0:]
print(bq)