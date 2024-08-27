import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_image_resampled(resample, image_original):
  try:
    image_resampled = image_original
    if resample == True:
      image_resampled = ants.resample_image(
        image = image_original, 
        resample_params = (256,256,256), 
        use_voxels = True, 
        interp_type = 4
      )
      if debug:
        image_resampled.plot(black_bg=False, nslices=12, ncol=4)
    return image_resampled
  except Exception as e:
    return RAVERuntimeException(e)


