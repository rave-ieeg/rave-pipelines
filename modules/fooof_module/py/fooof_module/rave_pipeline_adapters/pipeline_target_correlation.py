# Common script to call when Python module is loaded
# For example, import packages that is needed to run the module.
# 
#
# import numpy

from .. import shared
from . import RAVERuntimeException

def pipeline_target_correlation(input_data):
  try:
    import numpy as np
    correlation = np.corrcoef(input_data['x'], input_data['y'])
    print(correlation)
    return correlation
  except Exception as e:
    return RAVERuntimeException(e)


