import random
import math

a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d_list = [c for _ in range(9)]
e_list = [d_list for _ in range(8)]
f_list = [e_list for _ in range(8)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
j = ''
for _ in range(5):
        if _ == 5:
            break
        j += i
k_dict = {39: j, 25: j, 15: j, 63: j, 34: j, 19: j, 99: j, 65: j, 35: j}
l_dict = {48: k_dict, 86: k_dict, 64: k_dict, 1: k_dict, 17: k_dict, 21: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = ''
for _ in range(5):
        if _ == 3:
            continue
        o += n
p = [o for _ in range(6)]
random.shuffle(p)
q = random.choice(p)
def r():
    return q
def s():
    return r()
t = s()
u = t[0:]
v = ''
for _ in range(4):
    for __ in range(4):
                v += u
w = v[0:]
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = [ab for _ in range(5)]
random.shuffle(ac)
ad = random.choice(ac)
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = ''
for _ in range(4):
    aj += ai
ak = ''
counterak = 0
while counterak < 4:
    al = ''
    counteral = 0
    while counteral < 3:
        am = ''
        counteram = 0
        while counteram < 3:
            am += al
            counteram += 1
            al += ak
            counteral += 1
        ak += aj
        counterak += 1
an = am + '.'
ao = ''
counterao = 0
while counterao < 3:
    ap = ''
    counterap = 0
    while counterap < 5:
        aq = ''
        counteraq = 0
        while counteraq < 2:
            aq += ap
            counteraq += 1
            ap += ao
            counterap += 1
        ao += an
        counterao += 1
def ar():
    return aq
def at():
    return ar()
au = at()
print(au)