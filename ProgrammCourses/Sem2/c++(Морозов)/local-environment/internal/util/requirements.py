import importlib.util
import subprocess
import sys
from .log import info, error


def check(requirements):
  """Check that all the requirements are installed
  :param: requirements: list of required modules as strings
  :return: False on failure, True on success (all modules are present)
  """
  for r in requirements:
    spec = importlib.util.find_spec(r)
    if spec is None:
      return False
  return True


def install(requirements):
  """Run pip with current python executable and install the requirements
  :param: requirements: list of required libraries as strings
  :return: False on failure, True on success
  """
  for r in requirements:
    try:
      info("Installing requirement: {}".format(r))
      subprocess.check_call([sys.executable, "-m", "pip", "install", r])
    except subprocess.CalledProcessError as e:
      error("Error installing requirement {}: {}".format(r, e))
      return False
  return True


def assert_requirements(requirements):
  """Check if requirements are present and install if needed. On fail kills the script.
  :param: requirements: list of required libraries as strings
  """
  if not check(requirements) and not install(requirements):
    error("Requirements aren't present and unable to install them. Quitting.")
    sys.exit(1)