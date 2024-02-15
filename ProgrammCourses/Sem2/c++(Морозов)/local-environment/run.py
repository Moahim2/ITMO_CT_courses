#!/usr/bin/env python3
import os
import sys
from internal.util import env, dependencies_need_updates, warning, error
from internal.arguments import create_parser


def run_image(args):
  """Run docker container with local repository path
  mapped to the actual container path.
  """
  path = os.path.realpath(args.repo)
  if not os.path.exists(path):
    error("Specified repository path doesn't exist.")
    sys.exit(1)
  args.path = path
  args.func(args)


if __name__ == '__main__':
  # Check if newer dependencies are available
  if dependencies_need_updates(env.dependencies):
    warning("Updates available for dependencies. Please run build.py.")
    sys.exit(0)
  parser = create_parser()
  run_image(parser.parse_args())
