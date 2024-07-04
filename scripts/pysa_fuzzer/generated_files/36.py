import random
import math
a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = (d, d, d)
f, g, h = e
i = f + g + h
j = ''
for _ in range(6):
        if _ == 2:
            continue
        j += i
def k():
    return j
def l():
    return k()
m = l()
n = ''
countern = 0
while countern < 2:
    o = ''
    countero = 0
    while countero < 4:
        p = ''
        counterp = 0
        while counterp < 3:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
q = [p for _ in range(8)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(5):
    t = ''
    for _ in range(2):
        u = ''
        for _ in range(4):
            u += t
            t += s
        s += r
v_dict = {50: u, 58: u, 47: u, 76: u, 20: u, 20: u, 77: u, 73: u, 88: u, 49: u}
w_dict = {50: v_dict, 23: v_dict, 15: v_dict, 52: v_dict, 49: v_dict, 45: v_dict, 43: v_dict, 78: v_dict, 35: v_dict}
x_dict = {66: w_dict, 67: w_dict, 62: w_dict, 54: w_dict, 83: w_dict, 28: w_dict, 13: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = random.choice(list(z.values()))
ab = aa + '.'
ac = ab + '5'
ad = ac + '4'
ae = ad + '.'
af_dict = {2: ae, 76: ae, 49: ae, 38: ae, 90: ae, 45: ae}
ag_dict = {15: af_dict, 49: af_dict, 94: af_dict, 69: af_dict, 62: af_dict, 92: af_dict, 37: af_dict, 87: af_dict, 24: af_dict, 98: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = [ai for _ in range(5)]
random.shuffle(aj)
ak = random.choice(aj)
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = [ap for _ in range(8)]
random.shuffle(aq)
ar = random.choice(aq)
at = [ar for _ in range(6)]
random.shuffle(at)
au = random.choice(at)
av_set = {au, au, au, au, au, au, au}
av = random.choice(list(av_set))
aw = f'string {av}'
print(aw)