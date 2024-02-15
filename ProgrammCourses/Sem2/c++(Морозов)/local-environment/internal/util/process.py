import subprocess
import sys


def run_with_stdout(cmd):
  """Execute command with output from the process sent to stdout
  :param: cmd: Command to execute
  :return: Return code of the process
  """
  process = subprocess.run(
    cmd,
    stdout=sys.stdout,
    stderr=subprocess.STDOUT,
    shell=True
  )
  return process.returncode
