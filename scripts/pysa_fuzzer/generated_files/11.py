import random
import math
ot = input()
if ot == '6':
    ou = ot + ' c1'
elif ot == '20':
    ou = ot + ' c2'
else:
    ou = ot + ' c3'
if ou == '2':
    ov = ou + ' c1'
elif ou == '17':
    ov = ou + ' c2'
else:
    ov = ou + ' c3'
ow = ov[0:]
def ox():
    return ow
def oy():
    return ox()
def oz():
    return oy()
pa = oz()
pb_set = {pa, pa, pa, pa, pa, pa, pa, pa}
pb = random.choice(list(pb_set))
pc = ''
for _ in range(2):
    for __ in range(3):
                pc += pb
pd = f'string {pc}'
pe = pd + '1'
pf = pe + '8'
pg_list = [pf for _ in range(4)]
ph_list = [pg_list for _ in range(5)]
pi_list = [ph_list for _ in range(10)]
pj = random.choice(pi_list)
pk = random.choice(pj)
pl = random.choice(pk)
pm_dict = {41: pl, 52: pl, 59: pl, 24: pl, 81: pl, 79: pl}
pn_dict = {95: pm_dict, 83: pm_dict, 20: pm_dict, 64: pm_dict, 66: pm_dict}
po = random.choice(list(pn_dict.values()))
pp = random.choice(list(po.values()))
pq = (pp, pp, pp)
pr, ps, pt = pq
pu = pr + ps + pt
pv = [pu for _ in range(8)]
random.shuffle(pv)
pw = random.choice(pv)
px = pw[0:]
py = f'string {px}'
def pz():
    return py
qa = pz()
qb = f'string {qa}'
qc = f'string {qb}'
qd = qc + '.'
print(qd)