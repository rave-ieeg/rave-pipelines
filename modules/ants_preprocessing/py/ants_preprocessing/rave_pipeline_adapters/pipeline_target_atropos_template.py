import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_atropos_template(transforms):
  try:
    atropos_template = antspynet.deep_atropos(
      t1 = transforms["skull_strip"],
      do_preprocessing = False, 
      use_spatial_priors = True, 
      verbose = True)
    return atropos_template
  except Exception as e:
    return RAVERuntimeException(e)


