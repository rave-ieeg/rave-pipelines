import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_atropos_native(image_resampled, atropos_template, transforms):
  try:
    transform_list = transforms['template_transforms']['invtransforms']
    atropos_native = {}
    atropos_native['segmentation_image'] = ants.apply_transforms(
      fixed = image_resampled,
      moving = atropos_template['segmentation_image'],
      transformlist = transform_list,
      whichtoinvert = [True], interpolator="nearestNeighbor", verbose = False)
    
    probability_images = []
    atropos_native['probability_images'] = probability_images
    for img in atropos_template['probability_images']:
      probability_images.append(ants.apply_transforms(
        fixed = image_resampled,
        moving = img,
        transformlist = transform_list,
        whichtoinvert = [True], interpolator="linear", verbose = False
      ))
    return atropos_native
  except Exception as e:
    return RAVERuntimeException(e)


