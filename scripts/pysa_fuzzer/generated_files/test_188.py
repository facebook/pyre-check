import random
import math

a = input()
b = a + '.'
c = ''
counterc = 0
while counterc < 3:
    c += b
    counterc += 1
if c == c:
    f = c + 'c1'
elif c == '17':
    f = d + 'c2'
else:
    f = e + 'c3'
g_list = [f for _ in range(7)]
h_list = [g_list for _ in range(8)]
i_list = [h_list for _ in range(9)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m = ''
for _ in range(3):
    for __ in range(3):
                m += l
n_list = [m for _ in range(4)]
o_list = [n_list for _ in range(8)]
p_list = [o_list for _ in range(4)]
q = random.choice(p_list)
r = random.choice(q)
s = random.choice(r)
t_list = [s for _ in range(5)]
u = random.choice(t_list)
v_dict = {37: u, 27: u, 49: u, 16: u}
w = random.choice(list(v_dict.values()))
x = ''
counterx = 0
while counterx < 5:
    x += w
    counterx += 1
y = ''
for _ in range(8):
        if _ == 5:
            continue
        y += x
z_list = [y for _ in range(9)]
aa = random.choice(z_list)
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae = ''
for _ in range(6):
        if _ == 2:
            break
        ae += ad
af = [ae for _ in range(10)]
random.shuffle(af)
ag = random.choice(af)
ah_set = {ag, ag}
ah = random.choice(list(ah_set))
ai = ''
for _ in range(2):
    ai += ah
aj = ai + '.'
ak = f'string {aj}'
print(ak)