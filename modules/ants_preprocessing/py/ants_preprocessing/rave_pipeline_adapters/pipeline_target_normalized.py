import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_normalized(image_resampled, transforms, debug):
  try:
    normalized = ants.apply_transforms(
      fixed = image_resampled,
      moving = transforms['preprocessed_image'],
      transformlist = transforms['template_transforms']['invtransforms'],
      whichtoinvert = [True], interpolator="linear", verbose = True)
    if debug:
      normalized.plot(black_bg=False, nslices=12, ncol=4)
    return normalized
  except Exception as e:
    return RAVERuntimeException(e)


