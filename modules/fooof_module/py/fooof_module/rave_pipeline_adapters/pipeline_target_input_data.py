# Common script to call when Python module is loaded
# For example, import packages that is needed to run the module.
# 
#
# import numpy

from .. import shared
from . import RAVERuntimeException

def pipeline_target_input_data(sample_size, random_generator):
  try:
    import numpy as np
    
    # sample size as integer
    sample_s = int(sample_size)
    
    if sample_s <= 0:
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
    
    input_data = {
      'x' : generator(sample_s),
      'y' : generator(sample_s)
    }
    return input_data
  except Exception as e:
    return RAVERuntimeException(e)


