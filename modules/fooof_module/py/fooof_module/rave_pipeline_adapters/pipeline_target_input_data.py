# Common script to call when Python module is loaded
# For example, import packages that is needed to run the module.
# 
#
import numpy as np
from fooof_module.logger import log_info

from .. import shared
from . import RAVERuntimeException

def pipeline_target_input_data(sample_size, random_generator):
  try:
    # sample size as integer
    sample_size = int(sample_size)
    
    if sample_size <= 0:
      raise Exception(f"Sample size should be positive")
    
    # get random number generator
    if random_generator == "randn":
      generator = np.random.randn
    elif random_generator == "randint":
      generator = np.random.randint
    elif random_generator == "rand":
      generator = np.random.rand
    else:
      raise Exception(f"Unknown random number generator { generator_name }")
    
    
    log_info(f"Generating sample data with function np.random.{ random_generator } and sample size {sample_size}")
    
    input_data = {
      'x' : generator(sample_size),
      'y' : generator(sample_size)
    }
    return input_data
  except Exception as e:
    return RAVERuntimeException(e)


