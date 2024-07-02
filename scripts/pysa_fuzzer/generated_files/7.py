import random
import math
nv = input()
nw_list = [nv for _ in range(7)]
nx = random.choice(nw_list)
ny_list = [nx for _ in range(4)]
nz_list = [ny_list for _ in range(6)]
oa_list = [nz_list for _ in range(6)]
ob = random.choice(oa_list)
oc = random.choice(ob)
od = random.choice(oc)
oe = od + '9'
of = oe + '6'
og = ''
for _ in range(3):
    oh = ''
    for _ in range(2):
        oi = ''
        for _ in range(5):
            oi += oh
            oh += og
        og += of
oj = oi + '1'
ok = oj + '1'
ol = [ok for _ in range(6)]
random.shuffle(ol)
om = random.choice(ol)
on = ''
for _ in range(8):
        if _ == 4:
            break
        on += om
oo = f'string {on}'
op_list = [oo for _ in range(6)]
oq = random.choice(op_list)
os = oq + '9'
ot = os + '6'
ou = ot + '6'
ov = ou + '.'
ow = f'string {ov}'
def ox():
    return ow
def oy():
    return ox()
oz = oy()
if oz == '7':
    pa = oz + ' c1'
elif oz == '20':
    pa = oz + ' c2'
else:
    pa = oz + ' c3'
pb = f'string {pa}'
pc = [pb for _ in range(9)]
random.shuffle(pc)
pd = random.choice(pc)
if pd == '8':
    pe = pd + ' c1'
elif pd == '15':
    pe = pd + ' c2'
else:
    pe = pd + ' c3'
pf = (pe, pe, pe)
pg, ph, pi = pf
pj = pg + ph + pi
pk = f'string {pj}'
pl = ''
for _ in range(3):
    for __ in range(3):
                pl += pk
pm = (pl, pl, pl)
pn, po, pp = pm
pq = pn + po + pp
pr = [pq for _ in range(8)]
random.shuffle(pr)
ps = random.choice(pr)
pt_set = {ps, ps, ps, ps}
pt = random.choice(list(pt_set))
pu = [pt for _ in range(7)]
random.shuffle(pu)
pv = random.choice(pu)
pw_set = {pv, pv, pv, pv, pv, pv, pv}
pw = random.choice(list(pw_set))
px = pw[0:]
py = px + '.'
print(py)