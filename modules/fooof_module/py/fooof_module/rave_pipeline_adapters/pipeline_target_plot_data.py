# Common script to call when Python module is loaded
# For example, import packages that is needed to run the module.
# 
#
# import numpy

from .. import shared
from . import RAVERuntimeException

def pipeline_target_plot_data(input_data):
  try:
    import numpy as np
    plot_data = np.corrcoef(input_data['x'], input_data['y'])
    return plot_data
  except Exception as e:
    return RAVERuntimeException(e)


