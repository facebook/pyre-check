# Pattern One: 


def f0():
    if random.randint(1, 3) == 1:
        return input()
    else:
        return f0()
        

def f2():
    if random.randint(1, 3) == 1:
        return f0()
    else:
        return f2()
        

def f1(x):
    if random.randint(1, 3) == 1:
        return print(x)
    else:
        return f1(x)
        

def f3(x):
    if random.randint(1, 3) == 1:
        return f1(x)
    else:
        return f3(x)
        
f3(f2())


# pattern two 


def f0(x):
    if random.randint(1, 3) == 1:
        return x
    else:
        return f0(x)
        

def f2(x):
    if random.randint(1, 3) == 1:
        return x
    else:
        return f2(x)
        

def f1(x):
    if random.randint(1, 3) == 1:
        return print(x)
    else:
        return f1(x)
        

def f3(x):
    if random.randint(1, 3) == 1:
        return f1(x)
    else:
        return f3(x)
        
f3(f2(f0(input())))


# pattern three 

def f0(x, y):
    if random.randint(1, 3) == 1:
        return x + y
    else:
        return f0(x, y)
    

def f2(x, y):
    if random.randint(1, 3) == 1:
        return x + y
    else:
        return f2(x, y)
    

def f1(x):
    if random.randint(1, 3) == 1:
        return print(x)
    else:
        return f1(x)
    

def f3(x):
    if random.randint(1, 3) == 1:
        return f1(x)
    else:
        return f3(x)
    
f3(f2(f0(input(), 'temp'), 'temp'))


# pattern four 

def f0(x):
    y = x * 2
    if random.randint(1, 3) == 1:
        return y
    else:
        return f0(y)
    

def f2(x):
    y = x * 2
    if random.randint(1, 3) == 1:
        return y
    else:
        return f2(y)
    

def f1(x):
    if random.randint(1, 3) == 1:
        return print(x)
    else:
        return f1(x)
    

def f3(x):
    if random.randint(1, 3) == 1:
        return f1(x)
    else:
        return f3(x)
    
f3(f2(f0(input())))


# example six 


def f0(x):
    if (y := random.randint(1, 3)) == 1:
        return x + str(y)
    else:
        return f0(x)
    

def f2(x):
    if (y := random.randint(1, 3)) == 1:
        return x + str(y)
    else:
        return f2(x)
    

def f1(x):
    if (y := random.randint(1, 3)) == 1:
        return print(x + str(y))
    else:
        return f1(x)
    

def f3(x):
    if (y := random.randint(1, 3)) == 1:
        return f1(x + str(y))
    else:
        return f3(x)
    
f3(f2(f0(input())))


# example 7 


def f0(x):
    if (z := x.count('a')) > 2:
        return x.replace('a', 'arep')
    else:
        return f0(x + 'a')
    

def f2(x):
    if (z := x.count('a')) > 2:
        return x.replace('a', 'arep')
    else:
        return f2(x + 'a')
    

def f1(x):
    if (z := x.count('z')) > 1:
        return print(x.replace('a', 'arep'))
    else:
        return f1(x + 'z')
    

def f3(x):
    if (z := x.count('z')) > 1:
        return f1(x.replace('a', 'arep'))
    else:
        return f3(x + 'z')
    
f3(f2(f0(input())))


# example 8

def f0(x):
    hashmap = {char: ord(char) for char in x}
    if (value := hashmap.get('a')) is not None:
        return chr(value + 1) + x
    else:
        return f0(x + 'a')
    

def f2(x):
    hashmap = {char: ord(char) for char in x}
    if (value := hashmap.get('a')) is not None:
        return chr(value + 1) + x
    else:
        return f2(x + 'a')
    

def f1(x):
    hashmap = {char: ord(char) for char in x}
    if (value := hashmap.get('z')) is not None:
        return print(chr(value - 1) + x)
    else:
        return f1(x + 'z')
    

def f3(x):
    hashmap = {char: ord(char) for char in x}
    if (value := hashmap.get('z')) is not None:
        return f1(chr(value - 1) + x)
    else:
        return f3(x + 'z')
    
f3(f2(f0(input())))


# pattern 9 

def f0(x):
    hashmap = {char: x.count(char) for char in set(x)}
    if (count := hashmap.get('a', 0)) > 2:
        return 'b' * count + x
    else:
        return f0(x + 'a')
    

def f2(x):
    hashmap = {char: x.count(char) for char in set(x)}
    if (count := hashmap.get('a', 0)) > 2:
        return 'b' * count + x
    else:
        return f2(x + 'a')
    

def f1(x):
    hashmap = {char: x.count(char) for char in set(x)}
    if (count := hashmap.get('z', 0)) > 1:
        return print('y' * count + x)
    else:
        return f1(x + 'z')
    

def f3(x):
    hashmap = {char: x.count(char) for char in set(x)}
    if (count := hashmap.get('z', 0)) > 1:
        return f1('y' * count + x)
    else:
        return f3(x + 'z')
    
f3(f2(f0(input())))