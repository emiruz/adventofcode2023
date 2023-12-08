import math
from functools import reduce

def lcm(numbers):
    def lcm_pair(a, b):
        return abs(a * b) // math.gcd(a, b)
    
    return reduce(lcm_pair, numbers)




Xs = [14893,22199,20513,12083,13207,16579]
Xs = [14893,16579]
print(lcm(Xs))
