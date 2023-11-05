import math


primes = lambda n: [x for x in range(2, n) if len([y for y in range(2, int(math.sqrt(x))) if x % y == 0]) == 0]

qsort = lambda cmp, l: [] if not l else qsort(cmp, [i for i in l[1:] if cmp(l[0], i) < 0]) + [l[0]] + qsort(cmp, [i for i in l[1:] if cmp(l[0], i) >= 0])


print(primes(100))
print(qsort((lambda x,y: y-x), [10,5,7,14,3,0,19,6]))





# more readable :)

def qsort1(cmp, l):
  return [] if not l else qsort1(cmp, [i for i in l[1:] if cmp(l[0], i) < 0]) + [l[0]] + qsort1(cmp, [i for i in l[1:] if cmp(l[0], i) >= 0])

def qsort2(cmp, l):
  if not l: return []
  return qsort2(cmp, [i for i in l[1:] if cmp(l[0], i) < 0]) + [l[0]] + qsort2(cmp, [i for i in l[1:] if cmp(l[0], i) >= 0])

def qsort3(cmp, l):
  if not l:
    return []
  pivot = l[0]
  l = l[1:]
  return qsort3(cmp, [i for i in l if cmp(pivot, i) < 0]) + [pivot] + qsort3(cmp, [i for i in l if cmp(pivot, i) >= 0])

def qsort4(cmp, l):
  if not l:
    return []
  pivot = l[0]
  l = l[1:]
  smaller = qsort4(cmp, [i for i in l if cmp(pivot, i) < 0])
  bigger = qsort4(cmp, [i for i in l if cmp(pivot, i) >= 0])
  return smaller + [pivot] + bigger

print(qsort1((lambda x,y: y-x), [10,5,7,14,3,0,19,6]))
print(qsort2((lambda x,y: y-x), [10,5,7,14,3,0,19,6]))
print(qsort3((lambda x,y: y-x), [10,5,7,14,3,0,19,6]))
print(qsort4((lambda x,y: y-x), [10,5,7,14,3,0,19,6]))
