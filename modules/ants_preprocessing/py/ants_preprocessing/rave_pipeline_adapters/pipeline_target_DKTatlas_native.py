import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_DKTatlas_native(transforms, DKTatlas_template, image_resampled, debug):
  try:
    DKTatlas_native = ants.apply_transforms(
      fixed = image_resampled,
      moving = DKTatlas_template,
      transformlist=transforms['template_transforms']['invtransforms'],
      whichtoinvert = [True], interpolator="nearestNeighbor", verbose = True)
    if debug:
      DKTatlas_native.plot(cmap = "Set2", nslices=12, ncol=4)
    return DKTatlas_native
  except Exception as e:
    return RAVERuntimeException(e)


