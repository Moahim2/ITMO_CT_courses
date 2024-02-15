import os
import sys
from internal.util import info, error, warning, run_docker, run_with_stdout


def test(args):
  info("Running docker for tests")
  if run_docker(path=args.path, act="test"):
    info("Test success! Woohoo! Go ahead and submit this badboi to github :)")
  else:
    error("Tests failed :(")


def build(args):
  info("Running build inside docker")
  if run_docker(path=args.path, act="build"):
    info("Successfully built the project!")
  else:
    error("Build fail")


def fmt(args):
  if args.clang_format_path:
    clang_format_path = os.path.realpath(args.clang_format_path)
    info("Formatting using custom clang-format file ({})"
         .format(clang_format_path))
  else:
    clang_format_path = os.path.join(args.path, ".clang-format")
    info("Formatting to remote clang-format")
  if not os.path.exists(clang_format_path):
    error(".clang-format file doesn't exist")
    sys.exit(1)
  run_docker(path=args.path, act="fmt /format",
             volumes=[
               clang_format_path + ":{{container_repo_path}}/.clang-format"])


def checkfmt(args):
  clang_format_path = os.path.join(args.path, ".clang-format")
  if not os.path.exists(clang_format_path):
    error(".clang-format file doesn't exist")
    sys.exit(1)
  if run_docker(path=args.path, act="checkfmt"):
    info("Formatting is correct for all files!")
  else:
    warning("Wrong formatting in some files, run fmt...")


def send(args):
  info("Preparing repo for push to github")
  test(args)
  fmt(args)
  info("All good, pushing to github")

  oldpwd = os.getcwd()
  os.chdir(args.path)
  run_with_stdout("git add .")
  run_with_stdout("git commit -m \"(local-env) test and format\"")
  run_with_stdout("git push")
  os.chdir(oldpwd)


def add_actions(parser):
  actions_parser = parser.add_subparsers(title="Actions", required=True,
                                         dest="action")

  test_parser = actions_parser.add_parser(
    "test", help="Run tests", description="Run build, then test without and"
                                          " with ASAN/USAN")
  test_parser.set_defaults(func=test)

  build_parser = actions_parser.add_parser(
    "build", help="Run build", description="Only run build")
  build_parser.set_defaults(func=build)

  fmt_parser = actions_parser.add_parser(
    "fmt", help="Clang-Format the repo",
    description="Format code in your repo using remote clang-format"
  )
  fmt_parser.add_argument("clang_format_path",
                          help="Path to clang-format file which should be used"
                               " instead of the remote one for formatting",
                          nargs='?', default=None)
  fmt_parser.set_defaults(func=fmt)

  checkfmt_parser = actions_parser.add_parser(
    "checkfmt", help="Check formatting",
    description="Check the formatting of your repo using remote clang-format")
  checkfmt_parser.set_defaults(func=checkfmt)

  send_parser = actions_parser.add_parser(
    "send", help="Test, format and push",
    description="Run tests, if successful then format and push to remote")
  send_parser.set_defaults(func=send)
