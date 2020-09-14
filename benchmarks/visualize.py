import matplotlib.patches as mpatches
import scipy.stats
from scipy.stats import sem
import json
import numpy as np
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import sys
import os.path
import statsmodels.stats.weightstats as sm

DELTA = 1.0  # Tolerance for the TOST.
ALPHA = 0.05  # p-value threshold.
FILE_NAME = 'data/run.json'
if len(sys.argv) > 1:
    FILE_NAME = sys.argv[1]
FILE_NAME = os.path.normpath(FILE_NAME)
data = json.load(open(FILE_NAME))
df1 = pd.DataFrame(data)
df = pd.DataFrame([(d, *(tup[1:])) for tup in df1.itertuples()
                   for d in tup.fpsData])
df.columns = ['frame'] + list(df1.columns)
bench_names = list(set(list(df['bench_name'])))
for bench in sorted(bench_names):
    data_raw = df.loc[(df['shader'] == 'raw') & (
        df['bench_name'] == bench)]['frame']
    data_default = df.loc[(df['shader'] ==
                           'default') & (df['bench_name'] == bench)]['frame']
    data_raw, data_default = list(data_raw), list(data_default)

    print(bench)
    print(f"Means  : "
          f"GLSL {np.mean(data_raw):.2f} ± {sem(data_raw):.2f}  "
          f"Gator {np.mean(data_default):.2f} ± {sem(data_default):.2f}")
    print(f" Diff  : {np.mean(data_raw) - np.mean(data_default) : .2f}")

    # Difference of means tests.
    p_t = scipy.stats.ttest_ind(data_raw, data_default).pvalue
    p_wilcoxon = scipy.stats.wilcoxon(data_raw, data_default).pvalue
    print(f" Ttest : {p_t:.3f} " +
          ("*" if p_t < ALPHA else ""))
    print(f" Wilcox: {p_wilcoxon:.3f} " +
          ("*" if p_wilcoxon < ALPHA else ""))

    # TOST! We first perform two one-tailed t-tests where the null
    # hypothesis H0 is that the difference of means is *large* (so the
    # alternative hypothesis H1 is that the difference is *small*). By
    # using different alternative hypotheses and inverting the sign, we
    # get one test in each "direction."
    p_left = sm.ttest_ind(
        data_raw,
        data_default,
        alternative='larger',
        value=-DELTA,
    )[1]
    p_right = sm.ttest_ind(
        data_raw,
        data_default,
        alternative='smaller',
        value=DELTA,
    )[1]
    print(f" TOST  : smaller {p_left:.3f} larger {p_right:.3f}")

    # Now, if we have *rejected* both of the null hypotheses, we know
    # that the difference is smaller than DELTA in both directions.
    p_tost = max(p_left, p_right)  # Take the *worst* p-value.
    print(f" TOST p: {p_tost:.3f} " +
          ("*" if p_tost < ALPHA else ""))

    print('---------')

PLOT_TYPE = "bar"
if PLOT_TYPE == "violin":
    fig, ax = plt.subplots()
    sns.violinplot(
        ax=ax, data=df.loc[df['shader'] == 'default'], x="frame", y="bench_name", color='red')
    sns.violinplot(
        ax=ax, data=df.loc[df['shader'] == 'raw'], x="frame", y="bench_name", color='blue')
    plt.setp(ax.collections, alpha=.3)
    red_patch = mpatches.Patch(color='red', label='default')
    blue_patch = mpatches.Patch(color='blue', label='raw')

    plt.legend(handles=[blue_patch, red_patch])
    plt.show()
elif PLOT_TYPE == "bar":
    fig, ax = plt.subplots()
    g = sns.barplot(x="bench_name", y="frame",
                    data=df, hue="shader", ci="sd", ax=ax, hue_order=["default", "raw"])
    L = ax.legend()
    L.get_texts()[0].set_text('Gator')
    L.get_texts()[1].set_text('GLSL')
    plt.xlabel('Shader')
    plt.ylabel('fps')

    # plt.show()
    plt.savefig('evalresultsgator.pdf')
