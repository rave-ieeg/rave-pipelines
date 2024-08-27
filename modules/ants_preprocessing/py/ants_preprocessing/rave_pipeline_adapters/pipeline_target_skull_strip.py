import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_skull_strip(normalized, brain_mask, debug):
  try:
    skull_strip = normalized.clone()
    skull_strip[brain_mask == 0] = 0
    if debug:
      skull_strip.plot(black_bg=False, nslices=12, ncol=4)
    return skull_strip
  except Exception as e:
    return RAVERuntimeException(e)


