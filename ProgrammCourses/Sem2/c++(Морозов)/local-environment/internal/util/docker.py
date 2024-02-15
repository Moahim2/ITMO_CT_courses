import sys
from .env import variables
from .process import run_with_stdout
from .template import format_template


def run_docker(path, act, volumes=[]):
  return run_with_stdout(format_template(
      "docker run --rm --volume={{path}}:{{container_repo_path}} " +
      "".join(["--volume=" + v + " " for v in volumes]) +
      "cpp-env:1.0 {{act}}",
      {**variables, "act": act, "path": path})) == 0