import json
import pandas
guardian = json.loads(open('full.json').read())
#yougov = json.loads(open('constituency_detailed_results.json').read())
#yougov = pandas.read_csv('yougov.csv')
def parse(item):
  candidates = item['candidates']
  return [(item['name'], x['party'], x['percentageShare']) for x in candidates]

df = pandas.DataFrame(columns=['constituency','party','share'])

ret = [parse(x) for x in guardian if 'candidates' in x]
i = 0
for item in ret:
  for item1 in item:
    df.loc[i] = [item1[0], item1[1], item1[2]]
    i = i + 1
df.to_csv('results.csv')

