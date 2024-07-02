import random
import math
a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = [c for _ in range(8)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(3):
    g = ''
    for _ in range(5):
        h = ''
        for _ in range(4):
            h += g
            g += f
        f += e
i_list = [h for _ in range(8)]
j = random.choice(i_list)
if j == '5':
    k = j + ' c1'
elif j == '12':
    k = j + ' c2'
else:
    k = j + ' c3'
l = k + '4'
m = l + '1'
n = m + '7'
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 4:
        q = ''
        counterq = 0
        while counterq < 3:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
r = ''
for _ in range(2):
    for __ in range(5):
                r += q
s = f'string {r}'
t_list = [s for _ in range(6)]
u_list = [t_list for _ in range(9)]
v_list = [u_list for _ in range(8)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
for _ in range(10):
        if _ == 1:
            continue
        z += y
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = [ae for _ in range(9)]
random.shuffle(af)
ag = random.choice(af)
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = [aq for _ in range(10)]
random.shuffle(ar)
at = random.choice(ar)
au = ''
for _ in range(10):
        if _ == 4:
            continue
        au += at
av_dict = {26: au, 21: au, 87: au, 81: au, 69: au, 82: au, 23: au, 13: au, 70: au, 33: au}
aw_dict = {58: av_dict, 75: av_dict, 92: av_dict, 87: av_dict, 16: av_dict, 51: av_dict, 45: av_dict, 55: av_dict}
ax = random.choice(list(aw_dict.values()))
ay = random.choice(list(ax.values()))
print(ay)