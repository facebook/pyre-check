import random
a = input()
b = ''
counter = 0
while counter < 5:
    b += a
    counter += 1
if b == '3':
    c = b + ' condition1'
elif b == '18':
    c = b + ' condition2'
else:
    c = b + ' condition3'
d = (c, c, c)
e, f, g = d
h = e + f + g
i_set = {h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
counter = 0
while counter < 2:
    j += i
    counter += 1
k_list = [j for _ in range(5)]
k = random.choice(k_list)
l = (k, k, k)
m, n, o = l
p = m + n + o
q = f'Formatted string with {p}'
r_set = {q, q}
r = random.choice(list(r_set))
s = r[1:5]
t = s + '6'
u = ''
for _ in range(5):
    u += t
v = ''
for _ in range(3):
    for __ in range(5):
                v += u
w_set = {v, v, v, v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = w[2:4]
y = f'Formatted string with {x}'
z = y[1:5]
aa = ''
for _ in range(7):
        if _ == 4:
            continue
        aa += z
ab = ''
for _ in range(6):
        if _ == 2:
            continue
        ab += aa
ac = ab + ' concatenated'
ad_dict = {74: ac, 38: ac}
ad = random.choice(list(ad_dict.values()))
if ad == '5':
    ae = ad + ' condition1'
elif ad == '19':
    ae = ad + ' condition2'
else:
    ae = ad + ' condition3'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = f'Formatted string with {aj}'
al = ''
for _ in range(6):
        if _ == 5:
            break
        al += ak
am = al + ' concatenated'
an = am + '8'
ao_list = [an for _ in range(5)]
ao = random.choice(ao_list)
ap = ao + '3'
aq = f'Formatted string with {ap}'
ar = (aq, aq, aq)
at, au, av = ar
aw = at + au + av
ax = ''
for _ in range(2):
    ax += aw
ay = (ax, ax, ax)
az, ba, bb = ay
bc = az + ba + bb
print(bc)