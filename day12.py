import json

def fmap(f, data):
    if isinstance(data, list):
        return [f(x) for x in data]
    if isinstance(data, dict):
        return {k: f(v) for (k, v) in data.items()}
    return data

def cata(f, data):
    # First, we recurse inside all the values in data
    cata_on_f = lambda x: cata(f, x)
    recursed = fmap(cata_on_f, data)

    # Then, we apply f to the result
    return f(recursed)

def sumValues(data):
    if isinstance(data, dict):
        if 'red' in data.values(): # TODO, wrong, the 'red' is already mangled
            return sum(data.values())
        else:
            return 0
    if isinstance(data, list):
        return sum(data)
    try:
        return int(data)
    except:
        return 0

with open('input12') as f:
    j = json.load(f)
    print(cata(sumValues, j))
