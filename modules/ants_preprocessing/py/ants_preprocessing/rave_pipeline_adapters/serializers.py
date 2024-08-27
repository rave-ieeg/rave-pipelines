import os
import shutil
import ants
import antspynet
import json
import pandas

def ants_image_serializer(x, path):
  path2 = f"{path}.nii.gz"
  if x is None:
    if os.path.exists(path):
      os.remove(path)
    if os.path.exists(path2):
      os.remove(path2)
  else:
    x.to_file( path2 )
  return path2

def ants_image_unserializer(path, missing_ok = False):
  path2 = f"{path}.nii.gz"
  if os.path.exists(path2):
    return ants.image_read(path2)
  if not missing_ok:
    raise Exception(f"Cannot find image from path: {path2}")
  return None

def serialize_df(x, path):
  if isinstance(x, pandas.DataFrame):
    x.to_csv( path )
  elif os.path.exists( path ):
    os.remove( path )
  return path

def unserialize_df(path):
  if os.path.exists( path ):
    return pandas.read_csv( path )
  return None

def json_serializer(x, path):
  with open(path, mode = "w") as con:
    json.dump(x, con)
  return path

def json_unserializer(path):
  with open(path, mode = "r") as con:
    x = json.load(con)
    return x


def serialize_transforms(x, path):
  if not os.path.exists(path):
    os.makedirs(path)
  ants_image_serializer(x['preprocessed_image'], os.path.join(path, "preprocessed_image"))
  ants_image_serializer(x['brain_mask'], os.path.join(path, "brain_mask"))
  ants_image_serializer(x['skull_strip'], os.path.join(path, "skull_strip"))
  fwdtransforms = x['template_transforms']['fwdtransforms']
  fwdtransforms_len = len(fwdtransforms)
  fwdtransforms_new = []
  for ii in range(fwdtransforms_len):
    src_path = fwdtransforms[ii]
    dst_path = os.path.join(path, "fwdtransforms%d.mat" % ii)
    shutil.copyfile(src_path, dst_path)
    fwdtransforms_new.append("fwdtransforms%d.mat" % ii)
    
  invtransforms = x['template_transforms']['invtransforms']
  invtransforms_len = len(invtransforms)
  invtransforms_new = []
  for ii in range(invtransforms_len):
    src_path = invtransforms[ii]
    dst_path = os.path.join(path, "invtransforms%d.mat" % ii)
    shutil.copyfile(src_path, dst_path)
    invtransforms_new.append("invtransforms%d.mat" % ii)
  
  json_serializer({
    "fwdtransforms" : fwdtransforms_new,
    "invtransforms" : invtransforms_new
  }, os.path.join(path, "transform_paths.json"))
  return path
  
def unserialize_transforms(path):
  if not os.path.exists(path):
    raise Exception("Unable to unserialize `transforms`")
  transform_paths = json_unserializer(os.path.join(path, "transform_paths.json"))
  return {
    'preprocessed_image': ants_image_unserializer(os.path.join(path, 'preprocessed_image')),
    'brain_mask': ants_image_unserializer(os.path.join(path, 'brain_mask')),
    'skull_strip': ants_image_unserializer(os.path.join(path, 'skull_strip')),
    'template_transforms' : {
      'fwdtransforms': [os.path.abspath(os.path.join(path, x)) for x in transform_paths['fwdtransforms']],
      'invtransforms': [os.path.abspath(os.path.join(path, x)) for x in transform_paths['invtransforms']]
    }
  }

def serialize_atropos_template(x, path):
  if not os.path.exists(path):
    os.makedirs(path)
  ants_image_serializer(x = x["segmentation_image"], path = os.path.join(path, "segmentation_image"))
  # Label 0 :  background
  # Label 1 :  CSF
  # Label 2 :  gray matter
  # Label 3 :  white matter
  # Label 4 :  deep gray matter
  # Label 5 :  brain stem
  # Label 6 :  cerebellum
  ants_image_serializer(x = x["probability_images"][0], path = os.path.join(path, "probability_images_background"))
  ants_image_serializer(x = x["probability_images"][1], path = os.path.join(path, "probability_images_csf"))
  ants_image_serializer(x = x["probability_images"][2], path = os.path.join(path, "probability_images_gray_matter"))
  ants_image_serializer(x = x["probability_images"][3], path = os.path.join(path, "probability_images_white_matter"))
  ants_image_serializer(x = x["probability_images"][4], path = os.path.join(path, "probability_images_deep_gray_matter"))
  ants_image_serializer(x = x["probability_images"][5], path = os.path.join(path, "probability_images_brain_stem"))
  ants_image_serializer(x = x["probability_images"][6], path = os.path.join(path, "probability_images_cerebellum"))
  return path

def unserialize_atropos_template(path):
  if not os.path.exists(path):
    raise Exception("Unable to unserialize `atropos_template`")
  re = {}
  re['segmentation_image'] = ants_image_unserializer(path = os.path.join(path, "segmentation_image"))
  re['probability_images'] = []
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_background")))
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_csf")))
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_gray_matter")))
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_white_matter")))
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_deep_gray_matter")))
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_brain_stem")))
  re['probability_images'].append(
    ants_image_unserializer(path = os.path.join(path, "probability_images_cerebellum")))
  return re

# Used by RAVE to serialize/unserialize objects

def rave_serialize(x, path, name):
  if name == "transforms":
    return serialize_transforms(x, path)
  if name == "atropos_template" or name == "atropos_native":
    return serialize_atropos_template(x, path)
  if name == "cortical_thickness_regional_stats":
    return serialize_df(x, path)
  return ants_image_serializer(x, path)

def rave_unserialize(path, name):
  if name == "transforms":
    return unserialize_transforms(path)
  if name == "atropos_template" or name == "atropos_native":
    return unserialize_atropos_template(path)
  if name == "cortical_thickness_regional_stats":
    return unserialize_df(path)
  return ants_image_unserializer(path)


