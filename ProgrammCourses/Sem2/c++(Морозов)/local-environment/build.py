#!/usr/bin/env python3
import os
import sys
from internal.util import env, run_with_stdout, format_template_file,\
  assert_dependencies

SCRIPT_FOLDER = os.path.dirname(os.path.realpath(__file__))
APP_PATH = os.path.join(SCRIPT_FOLDER, 'app')


def path(file):
  """For script usage always choose to use the real path - simpler to debug
  after breaking something with new additions later"""
  return os.path.join(APP_PATH, file)


def preprocess():
  """Format the dockerfile with proper variables like local repository path and what-not.
  """
  format_template_file(path("Dockerfile"), path("Dockerfile.build"), env.variables)


def postprocess():
  """Remove the temporary files we created during preprocessing or building
  """
  os.remove(path("Dockerfile.build"))


def build_image():
  # For dockerfile temporarily switch folders
  oldpwd = os.getcwd()
  os.chdir(SCRIPT_FOLDER)
  run_with_stdout("docker image rm --force cpp-env:1.0")
  if run_with_stdout("docker image build . "
                     "--file app/Dockerfile.build --tag cpp-env:1.0") != 0:
    print("Docker build failed.")
    sys.exit(1)
  os.chdir(oldpwd)


if __name__ == '__main__':
  # Make sure that our dependencies are satisfied
  assert_dependencies(env.dependencies)
  preprocess()
  build_image()
  postprocess()
