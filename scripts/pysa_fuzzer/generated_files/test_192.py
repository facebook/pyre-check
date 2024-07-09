import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e_list = [d for _ in range(4)]
f_list = [e_list for _ in range(4)]
g = random.choice(f_list)
h = random.choice(g)
i = h + '1'
j = i + '4'
k = j[0:]
l = ''
for _ in range(5):
    m = ''
    for _ in range(4):
        m += l
        l += k
n = m + '.'
if n == n:
    q = n + 'c1'
elif n == '19':
    q = o + 'c2'
else:
    q = p + 'c3'
if q == q:
    t = q + 'c1'
elif q == '18':
    t = r + 'c2'
else:
    t = s + 'c3'
u_list = [t for _ in range(8)]
v_list = [u_list for _ in range(7)]
w_list = [v_list for _ in range(10)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
if z == z:
    ac = z + 'c1'
elif z == '14':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = ''
for _ in range(3):
    for __ in range(2):
                ad += ac
ae_dict = {28: ad, 77: ad, 34: ad, 52: ad, 62: ad, 86: ad, 48: ad}
af_dict = {5: ae_dict, 50: ae_dict, 40: ae_dict, 8: ae_dict, 38: ae_dict, 2: ae_dict, 42: ae_dict, 90: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai_dict = {13: ah, 29: ah, 9: ah, 80: ah, 50: ah, 67: ah}
aj_dict = {18: ai_dict, 62: ai_dict}
ak_dict = {36: aj_dict, 59: aj_dict, 36: aj_dict, 65: aj_dict, 27: aj_dict, 56: aj_dict, 88: aj_dict, 89: aj_dict, 98: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
an = random.choice(list(am.values()))
if an == an:
    aq = an + 'c1'
elif an == '16':
    aq = ao + 'c2'
else:
    aq = ap + 'c3'
ar = f'string {aq}'
at = ''
counterat = 0
while counterat < 2:
    at += ar
    counterat += 1
au = ''
counterau = 0
while counterau < 5:
    au += at
    counterau += 1
av = (au, au, au)
aw, ax, ay = av
az = aw + ax + ay
print(az)