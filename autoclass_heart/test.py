from pathlib import Path
import sys
import time

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import numpy as np
import pandas as pd



print("Python:", sys.version)
print("matplotlib:", matplotlib.__version__)
print("numpy:", np.__version__)
print("pandas:", pd.__version__)

import autoclasswrapper as wrapper
print("AutoClassWrapper:", wrapper.__version__)

version = sys.version_info
if not ((version.major >= 3) and (version.minor >= 6)):
    sys.exit("Need Python>=3.6")
    
#2
size = 100
sigma = 0.6
x = np.concatenate((np.random.normal(3, sigma, size), np.random.normal(4, sigma, size), np.random.normal(6, sigma, size)))
y = np.concatenate((np.random.normal(4, sigma, size), np.random.normal(0, sigma, size), np.random.normal(5, sigma, size)))
color = ["blue"]*size+["orange"]*size+["purple"]*size
name = ["id{:03d}".format(id) for id in range(size*3)]
df = pd.DataFrame.from_dict({"x":x, "y":y, "color":color})
df.index = name
df.index.name = "name"
df.head()



#3
plt.scatter(df["x"], df["y"], color=df["color"], s=10)
plt.xlabel("x")
plt.ylabel("y")
plt.xlim(0, 10)
plt.ylim(-5, 10);

#4
# verify all x are > 0
assert min(df["x"]) > 0

#5
#df["x"].to_csv("demo_real_scalar.tsv", sep="\t", header=True)
#df["y"].to_csv("demo_real_location.tsv", sep="\t", header=True)

#6
# Create object to prepare dataset.
clust = wrapper.Input()

# Load datasets from tsv files.
clust.add_input_data("heart_real.tsv", "real scalar")
clust.add_input_data("heart_discret.tsv", "discrete")

# Prepare input data:
# - create a final dataframe
# - merge datasets if multiple inputs
clust.prepare_input_data()

# Create files needed by AutoClass.
clust.create_db2_file()
clust.create_hd2_file()
clust.create_model_file()
clust.create_sparams_file()
clust.create_rparams_file()


#7
# Clean previous status file and results if a classification has already been performed.


# Search autoclass in path.
wrapper.search_autoclass_in_path()

# Create object to run AutoClass.
run = wrapper.Run()

# Prepare run script.
run.create_run_file()

# Run AutoClass.
run.run()


#8
timer = 0
step = 2
while not Path("autoclass-run-success").exists():
    timer += step
    sys.stdout.write("\r")
    sys.stdout.write(f"Time: {timer} sec.")
    sys.stdout.flush()
    time.sleep(step)

results = wrapper.Output()
results.extract_results()
results.aggregate_input_data()
results.write_cdt()
results.write_cdt(with_proba=True)
results.write_class_stats()
results.write_dendrogram()

#9


