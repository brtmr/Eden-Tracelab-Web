import random
import json

NUM_MACHINES = 32
MAX_TIME = 20000000000

MIN_LENGTH = 500000
MAX_LENGTH = 100*MIN_LENGTH

def to_ns(t):
    return t*1000000000


#generates a list of events for a single machine
def generate_machine(n):
#generate a start time,that is within the first 5 seconds
    res = []
    start = random.randint(0,500)/100.0
    t = to_ns(start)
    while t < MAX_TIME:
        end = t + random.randint(MIN_LENGTH, MAX_LENGTH)
        if end > MAX_TIME:
            end = MAX_TIME
        event = {}
        event['start'] = t
        event['end']= end
        event['state'] = random.randint(0,3)
        event['type_'] = 'M'
        event['id_'] = n
        t = end
        res.append(event)
    return res


def main():
    events = []
    for n in range(NUM_MACHINES):
        events+=generate_machine(n)
    res = {}
    res['events']= events
    res['starttime']= 0
    res['endtime']= MAX_TIME
    res['machines']= NUM_MACHINES
    print('dummy_data = ', end='')
    print(json.dumps(res))

if __name__=='__main__':
    main()
