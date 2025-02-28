# Common script to call when Python module is loaded
# For example, import packages that is needed to run the module.
# 
#
# import numpy

from .. import shared
from . import RAVERuntimeException

def pipeline_target_input_data():
  try:
    import numpy as np
    input_data = {
      'x' : np.random.rand(10),
      'y' : np.random.rand(10)
    }
    return input_data
  except Exception as e:
    return RAVERuntimeException(e)


