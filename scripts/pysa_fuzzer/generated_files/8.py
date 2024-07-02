import random
import math
kj = input()
def kk():
    return kj
def kl():
    return kk()
def km():
    return kl()
kn = km()
ko = kn + '.'
kp = ''
for _ in range(8):
        if _ == 1:
            break
        kp += ko
kq_set = {kp, kp, kp, kp, kp, kp, kp, kp, kp, kp}
kq = random.choice(list(kq_set))
kr = ''
for _ in range(9):
        if _ == 4:
            break
        kr += kq
ks_set = {kr, kr, kr, kr, kr, kr, kr, kr, kr}
ks = random.choice(list(ks_set))
kt = ''
for _ in range(9):
        if _ == 1:
            break
        kt += ks
ku = [kt for _ in range(5)]
random.shuffle(ku)
kv = random.choice(ku)
kw = ''
for _ in range(2):
    kx = ''
    for _ in range(2):
        kx += kw
        kw += kv
def ky():
    return kx
def kz():
    return ky()
la = kz()
lb = ''
for _ in range(9):
        if _ == 5:
            break
        lb += la
lc = ''
counterlc = 0
while counterlc < 4:
    ld = ''
    counterld = 0
    while counterld < 3:
        ld += lc
        counterld += 1
        lc += lb
        counterlc += 1
le = ''
for _ in range(5):
    for __ in range(2):
                le += ld
lf = ''
for _ in range(7):
        if _ == 5:
            break
        lf += le
lg = ''
for _ in range(7):
        if _ == 2:
            break
        lg += lf
lh = lg + '.'
li = lh + '.'
lj = [li for _ in range(5)]
random.shuffle(lj)
lk = random.choice(lj)
print(lk)