import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_cortical_thickness(atropos_native, calculate_cortical_thickness):
  try:
    cortical_thickness = None
    
    if calculate_cortical_thickness:
      # https://www.medrxiv.org/content/10.1101/2020.10.19.20215392v1.full
      kk_segmentation = ants.image_clone(atropos_native['segmentation_image'])
      kk_segmentation[kk_segmentation == 4] = 3
      gray_matter = atropos_native['probability_images'][2]
      white_matter = (
        atropos_native['probability_images'][3] + 
        atropos_native['probability_images'][4]
      )
      cortical_thickness = ants.kelly_kapowski(
        s=kk_segmentation, g=gray_matter, w=white_matter,
        its=45, r=0.025, m=1.5, x=0, verbose=1)
    return cortical_thickness
  except Exception as e:
    return RAVERuntimeException(e)


