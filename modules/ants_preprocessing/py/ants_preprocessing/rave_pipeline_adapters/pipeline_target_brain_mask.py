import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_brain_mask(image_resampled, transforms, debug):
  try:
    brain_mask = ants.apply_transforms(
      fixed = image_resampled,
      moving = transforms['brain_mask'],
      transformlist=transforms['template_transforms']['invtransforms'],
      whichtoinvert = [True], interpolator="linear", verbose = True)
    if debug:
      brain_mask.plot(black_bg=False, nslices=12, ncol=4)
    return brain_mask
  except Exception as e:
    return RAVERuntimeException(e)


