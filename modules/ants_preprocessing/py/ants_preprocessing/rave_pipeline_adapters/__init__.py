
class RAVERuntimeException(object):
  original_exception = None
  def __init__(self, e):
    if isinstance(e, Exception):
      self.original_exception = e
    elif isinstance(e, str):
      self.original_exception = Exception(e)
    else:
      self.original_exception = Exception('Unknown error')
  def __str__(self):
    return '{}: {}'.format(type(self.original_exception).__name__, self.original_exception)

from .serializers import rave_serialize
from .serializers import rave_unserialize

from .pipeline_target_image_original import pipeline_target_image_original as image_original
from .pipeline_target_image_resampled import pipeline_target_image_resampled as image_resampled
from .pipeline_target_transforms import pipeline_target_transforms as transforms
from .pipeline_target_normalized import pipeline_target_normalized as normalized
from .pipeline_target_brain_mask import pipeline_target_brain_mask as brain_mask
from .pipeline_target_skull_strip import pipeline_target_skull_strip as skull_strip
from .pipeline_target_atropos_template import pipeline_target_atropos_template as atropos_template
from .pipeline_target_atropos_native import pipeline_target_atropos_native as atropos_native
from .pipeline_target_DKTatlas_template import pipeline_target_DKTatlas_template as DKTatlas_template
from .pipeline_target_DKTatlas_native import pipeline_target_DKTatlas_native as DKTatlas_native
from .pipeline_target_cortical_thickness import pipeline_target_cortical_thickness as cortical_thickness
from .pipeline_target_DKT_propagated import pipeline_target_DKT_propagated as DKT_propagated
from .pipeline_target_cortical_thickness_regional_stats import pipeline_target_cortical_thickness_regional_stats as cortical_thickness_regional_stats
