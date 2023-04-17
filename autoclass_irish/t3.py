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


# Create object to prepare dataset.
clust = wrapper.Input()

# Load datasets from tsv files.
clust.add_input_data("diag_real.tsv", "real scalar")
clust.add_input_data("diag_discret.tsv", "discrete")

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




# Clean previous status file and results if a classification has already been performed.

# Search autoclass in path.
wrapper.search_autoclass_in_path()

# Create object to run AutoClass.
run = wrapper.Run()

# Prepare run script.
run.create_run_file()

# Run AutoClass.
run.run()
