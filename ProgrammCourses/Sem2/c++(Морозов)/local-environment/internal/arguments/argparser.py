import argparse
from .actions import add_actions


def create_parser():
  parser = argparse.ArgumentParser(
    description=
    "Test, compile, format your project before submitting it to github.\n"
    "Before running this, make sure you ran build.py in order to "
    "create the required docker container.",
    epilog="By @renbou with <3",
    formatter_class=argparse.RawDescriptionHelpFormatter
  )
  add_actions(parser)
  parser.add_argument("repo", help="Path to repository containing the project")
  return parser