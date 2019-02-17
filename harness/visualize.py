import scipy.stats
import json
import numpy as np
import seaborn
import matplotlib.pyplot as plt
import sys
import os.path

FILE_NAME = 'data/run.json'
if len(sys.argv) > 1:
    FILE_NAME = f'data/{sys.argv[1]}'
FILE_NAME = os.path.normpath(FILE_NAME)
data = json.load(open(FILE_NAME))

benchmarks = list({i['bench_name'] for i in data})
print(benchmarks)
assert(len(benchmarks)==1) # Visualization is only set up for 1 benchmark
shaders = list({i['shader'] for i in data})
print(shaders)
assert(len(shaders) == 2) # Visualization is currently only set up for comparing 2 shaders at a time
dataA = [i['fpsData'] for i in data if i['shader'] == shaders[0]]
dataB = [i['fpsData'] for i in data if i['shader'] == shaders[1]]
dataA = [j for i in dataA for j in i]
dataB = [j for i in dataB for j in i]
print(np.mean(dataA), np.mean(dataB))
print(scipy.stats.ttest_ind(dataA, dataB))
fig, ax = plt.subplots()
seaborn.violinplot(dataA, ax=ax, color='red')
seaborn.violinplot(dataB, ax=ax, color='blue')
plt.show()
