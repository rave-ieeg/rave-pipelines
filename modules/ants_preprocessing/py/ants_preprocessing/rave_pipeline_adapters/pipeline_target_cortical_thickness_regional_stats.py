import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_cortical_thickness_regional_stats(calculate_cortical_thickness, DKT_propagated, cortical_thickness):
  try:
    cortical_thickness_regional_stats = None
    
    if calculate_cortical_thickness:
      cortical_thickness_regional_stats = ants.label_stats(
        cortical_thickness, DKT_propagated)
    return cortical_thickness_regional_stats
  except Exception as e:
    return RAVERuntimeException(e)


