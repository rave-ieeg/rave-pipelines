import os
import ants
import antspynet

from .. import shared
from . import RAVERuntimeException

def pipeline_target_transforms(image_resampled):
  try:
    transforms = antspynet.preprocess_brain_image(
      image_resampled,
      truncate_intensity = (0.01, 0.99),
      brain_extraction_modality = "t1",
      template = "croppedMni152",
      template_transform_type = "antsRegistrationSyNRepro[a]",
      do_bias_correction = True,
      do_denoising = True
    )
    skull_strip_template = transforms['preprocessed_image'].clone()
    skull_strip_template[transforms['brain_mask'] == 0] = 0
    transforms["skull_strip"] = skull_strip_template
    return transforms
  except Exception as e:
    return RAVERuntimeException(e)


