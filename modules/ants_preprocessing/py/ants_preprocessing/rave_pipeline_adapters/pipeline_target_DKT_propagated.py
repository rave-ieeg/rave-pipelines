import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_DKT_propagated(DKTatlas_native, cortical_thickness, calculate_cortical_thickness):
  try:
    DKT_propagated = None
    
    if calculate_cortical_thickness:
      dtk_cortical_mask = ants.threshold_image(
        image=DKTatlas_native, low_thresh=1000, 
        high_thresh=3000, inval=1, outval=0)
      
      dtk = dtk_cortical_mask * DKTatlas_native
      
      kk_mask = ants.threshold_image(
        image=cortical_thickness, low_thresh=0,
        high_thresh = 0, inval = 0, outval = 1)
      
      DKT_propagated = ants.iMath(kk_mask, "PropagateLabelsThroughMask", kk_mask * dtk)
    return DKT_propagated
  except Exception as e:
    return RAVERuntimeException(e)


