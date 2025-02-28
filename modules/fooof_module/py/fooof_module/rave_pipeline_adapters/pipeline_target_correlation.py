# Common script to call when Python module is loaded
# For example, import packages that is needed to run the module.
# 
#
import numpy as np
from fooof_module.logger import log_info

from .. import shared
from . import RAVERuntimeException

def pipeline_target_correlation(input_data):
  try:
    correlation = np.corrcoef(input_data['x'], input_data['y'])
    log_info(f"Calculating correlation: { correlation }")
    return correlation
  except Exception as e:
    return RAVERuntimeException(e)


