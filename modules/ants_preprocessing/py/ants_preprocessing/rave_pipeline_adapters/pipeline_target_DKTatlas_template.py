import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_DKTatlas_template(transforms):
  try:
    DKTatlas_template = antspynet.desikan_killiany_tourville_labeling(
      t1 = transforms['skull_strip'],
      do_preprocessing = False, 
      return_probability_images = False,
      do_lobar_parcellation = False, 
      verbose = True 
    )
    return DKTatlas_template
  except Exception as e:
    return RAVERuntimeException(e)


