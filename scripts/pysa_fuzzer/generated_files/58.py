import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(2):
        c += b
        b += a
d_list = [c for _ in range(9)]
e_list = [d_list for _ in range(4)]
f = random.choice(e_list)
g = random.choice(f)
h = g + '.'
i = h + '7'
j = i + '5'
k = j + '4'
l = k + '.'
m = [l for _ in range(9)]
random.shuffle(m)
n = random.choice(m)
o = ''
for _ in range(4):
    p = ''
    for _ in range(2):
        p += o
        o += n
q = (p, p, p)
r, s, t = q
u = r + s + t
v_list = [u for _ in range(3)]
w_list = [v_list for _ in range(2)]
x_list = [w_list for _ in range(10)]
y = random.choice(x_list)
z = random.choice(y)
aa = random.choice(z)
ab_list = [aa for _ in range(5)]
ac = random.choice(ab_list)
ad = ac[0:]
def ae():
    return ad
af = ae()
ag = af + '.'
ah = ag + '3'
ai = ah + '9'
aj_list = [ai for _ in range(3)]
ak_list = [aj_list for _ in range(10)]
al = random.choice(ak_list)
am = random.choice(al)
an_list = [am for _ in range(3)]
ao_list = [an_list for _ in range(10)]
ap_list = [ao_list for _ in range(3)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = random.choice(ar)
if at == '5':
    au = at + ' c1'
elif at == '20':
    au = at + ' c2'
else:
    au = at + ' c3'
av_dict = {82: au, 98: au, 55: au, 71: au, 36: au, 36: au, 90: au}
aw = random.choice(list(av_dict.values()))
print(aw)