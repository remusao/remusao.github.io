import matplotlib
import matplotlib.pyplot as plt
import numpy as np

# matplotlib.rcParams.update({'font.size': 20})
labels = ['HTTPS E. (WASM)', 'HTTPS E. (JS)', 'Index']
chrome_means = [0.028, 0.0068, 0.0046]
firefox_means = [0.023, 0.0093, 0.0073]

x = np.arange(len(labels))  # the label locations
width = 0.35  # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(x - width/2, chrome_means, width, label='Chrome')
rects2 = ax.bar(x + width/2, firefox_means, width, label='Firefox')

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Decision time (milliseconds)')
ax.set_title('Average decision time per URL')
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.legend()


def autolabel(rects):
    """Attach a text label above each bar in *rects*, displaying its height."""
    for rect in rects:
        height = rect.get_height()
        ax.annotate('{}'.format(height),
                    xy=(rect.get_x() + rect.get_width() / 2, height),
                    xytext=(0, 1),  # 3 points vertical offset
                    textcoords="offset points",
                    ha='center', va='bottom')


autolabel(rects1)
autolabel(rects2)

fig.tight_layout()

plt.savefig('test.png')
plt.savefig('test.svg')
# plt.show()
