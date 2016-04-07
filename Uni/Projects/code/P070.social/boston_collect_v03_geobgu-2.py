import json
import datetime
from twarc import Twarc

# Collection end
period_end = datetime.datetime(2017, 12, 31, 23, 59, 59, 999999)

# Twitter API keys - geobgu2
t = Twarc(
  'JA5KZiEuU8HDIFDtLXwkHCpdx', 
  'NdGoBYXuYHbHOAInNHHumjz0xeCp8zEYfbm0RW0dzpvcRY8Ovc', 
  '2782755278-ARD36i5dPBU6fxRdgvomZoxuCOI3ewVVGPizZCf', 
  'ceN8O8yIVV2C7o6CJyLYYo3CNIm48Tnojpxj69pqqv36u'
  )

# Twitter stream request
t = t.filter(locations = "\-72.21437,41.19034,-69.64939,43.30924")

# Collect tweets
while datetime.datetime.now() < period_end: # Loop until collection period ends
    day_start = datetime.datetime.now()
    day_end = datetime.datetime(day_start.year, day_start.month, day_start.day, day_start.hour, 59, 59, 999999)
    fh = open("boston_geobgu2_" + day_start.strftime("%Y-%m-%d_%H:%M:%S") + ".json", "w")
    #fh.write("[")
    for tweet in t: # Loop until hour ends
        x = tweet
        try:
            if x["geo"] != None:
                print(x["text"])
                fh.write(json.dumps(x))
                fh.write("\n")
        except:
            continue
        if datetime.datetime.now() > day_end:
            #fh.write("]")
            fh.close()
            break


# Errors

#Traceback (most recent call last):
#  File "b.py", line 27, in <module>
#    if x["geo"] != None:
#KeyError: 'geo'


