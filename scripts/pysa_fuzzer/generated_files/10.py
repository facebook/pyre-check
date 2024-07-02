import random
import math
nf = input()
ng_dict = {93: nf, 92: nf, 85: nf}
nh_dict = {35: ng_dict, 99: ng_dict, 4: ng_dict, 70: ng_dict, 89: ng_dict}
ni_dict = {41: nh_dict, 98: nh_dict, 69: nh_dict, 13: nh_dict, 39: nh_dict, 37: nh_dict, 49: nh_dict}
nj = random.choice(list(ni_dict.values()))
nk = random.choice(list(nj.values()))
nl = random.choice(list(nk.values()))
nm = f'string {nl}'
nn = ''
for _ in range(5):
        if _ == 4:
            break
        nn += nm
if nn == '4':
    no = nn + ' c1'
elif nn == '11':
    no = nn + ' c2'
else:
    no = nn + ' c3'
np = [no for _ in range(8)]
random.shuffle(np)
nq = random.choice(np)
nr = ''
for _ in range(9):
        if _ == 3:
            continue
        nr += nq
ns_list = [nr for _ in range(4)]
nt_list = [ns_list for _ in range(7)]
nu_list = [nt_list for _ in range(8)]
nv = random.choice(nu_list)
nw = random.choice(nv)
nx = random.choice(nw)
ny = ''
counterny = 0
while counterny < 3:
    ny += nx
    counterny += 1
nz = ny + '.'
oa = nz + '5'
ob = oa + '8'
oc = ob + '1'
od = ''
for _ in range(4):
    oe = ''
    for _ in range(2):
        oe += od
        od += oc
of = [oe for _ in range(7)]
random.shuffle(of)
og = random.choice(of)
oh = ''
for _ in range(2):
    oh += og
oi_set = {oh, oh, oh, oh, oh, oh}
oi = random.choice(list(oi_set))
oj = ''
for _ in range(10):
        if _ == 4:
            break
        oj += oi
ok = oj + '1'
ol = ok + '2'
om = (ol, ol, ol)
on, oo, op = om
oq = on + oo + op
os = ''
counteros = 0
while counteros < 2:
    os += oq
    counteros += 1
print(os)