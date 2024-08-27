import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_image_original(image_path_normalized, debug):
  try:
    image_original = ants.image_read(image_path_normalized)
    if debug:
      image_original.plot(black_bg=False, nslices=12, ncol=4)
    return image_original
  except Exception as e:
    return RAVERuntimeException(e)


