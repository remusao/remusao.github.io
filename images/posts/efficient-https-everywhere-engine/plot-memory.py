import matplotlib
import matplotlib.pyplot as plt
import numpy as np

# matplotlib.rcParams.update({'font.size': 20})
labels = ['HTTPS E. (WASM)', 'HTTPS E. (JS)', 'Index', 'Index (compression)']
chrome_means = [20, 33.7, 7, 5]
# firefox_means = [540, 305, 45]

x = np.arange(len(labels))  # the label locations
width = 0.35  # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(x, chrome_means, 0.6, label='Chrome Memory Dev Tool')
# rects2 = ax.bar(x + width/2, firefox_means, width, label='Firefox')

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Memory (MB)')
ax.set_title('Memory usage of rulesets')
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.legend()


def autolabel(rects):
    """Attach a text label above each bar in *rects*, displaying its height."""
    for rect in rects:
        height = rect.get_height()
        if height != 0:
            ax.annotate('{}'.format(height),
                        xy=(rect.get_x() + rect.get_width() / 2, height),
                        xytext=(0, 1),  # 3 points vertical offset
                        textcoords="offset points",
                        ha='center', va='bottom')


autolabel(rects1)
# autolabel(rects2)

fig.tight_layout(pad=3)

plt.savefig('test.png')
plt.savefig('test.svg')
plt.show()
