import random
import math
hc = input()
def hd():
    return hc
def he():
    return hd()
hf = he()
hg_dict = {85: hf, 85: hf, 6: hf, 42: hf, 7: hf, 61: hf, 68: hf, 100: hf, 68: hf}
hh = random.choice(list(hg_dict.values()))
hi = [hh for _ in range(5)]
random.shuffle(hi)
hj = random.choice(hi)
hk = ''
counterhk = 0
while counterhk < 5:
    hl = ''
    counterhl = 0
    while counterhl < 4:
        hm = ''
        counterhm = 0
        while counterhm < 3:
            hm += hl
            counterhm += 1
            hl += hk
            counterhl += 1
        hk += hj
        counterhk += 1
hn = f'string {hm}'
def ho():
    return hn
def hp():
    return ho()
hq = hp()
hr = (hq, hq, hq)
hs, ht, hu = hr
hv = hs + ht + hu
hw = ''
for _ in range(3):
    for __ in range(4):
                hw += hv
hx = ''
counterhx = 0
while counterhx < 3:
    hx += hw
    counterhx += 1
hy = ''
counterhy = 0
while counterhy < 3:
    hy += hx
    counterhy += 1
hz_dict = {35: hy, 17: hy, 55: hy, 97: hy, 51: hy, 49: hy, 9: hy}
ia = random.choice(list(hz_dict.values()))
ib = ''
for _ in range(5):
    for __ in range(2):
                ib += ia
ic = ib[0:]
id_dict = {37: ic, 52: ic, 33: ic, 14: ic, 91: ic}
ie_dict = {49: id_dict, 77: id_dict, 55: id_dict, 35: id_dict, 50: id_dict, 58: id_dict}
ig_dict = {95: ie_dict, 64: ie_dict}
ih = random.choice(list(ig_dict.values()))
ii = random.choice(list(ih.values()))
ij = random.choice(list(ii.values()))
ik = ''
counterik = 0
while counterik < 4:
    ik += ij
    counterik += 1
il = ik + '.'
im_dict = {34: il, 71: il, 45: il, 61: il, 50: il, 49: il, 34: il, 29: il, 60: il}
io_dict = {13: im_dict, 81: im_dict, 61: im_dict}
ip_dict = {7: io_dict, 74: io_dict, 92: io_dict, 81: io_dict}
iq = random.choice(list(ip_dict.values()))
ir = random.choice(list(iq.values()))
it = random.choice(list(ir.values()))
iu = ''
for _ in range(4):
    iu += it
print(iu)