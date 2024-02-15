import sys


def supports_color():
  """Check if terminal supports colors"""
  return hasattr(sys.stdout, "isatty") and sys.stdout.isatty() \
         and sys.platform != "win32"


ansi = {
  "RESET": "\x1b[0m",
  "WARNING": "\x1b[38;5;208m",
  "ERROR": "\x1b[31m",
  "BOLD": "\x1b[1m",
  "INFO": "\x1b[38;5;118m"
}
if not supports_color():
  for key in ansi:
    ansi[key] = ''


def warning(message):
  print(ansi["BOLD"] + ansi["WARNING"] + "[-] " + ansi["RESET"]
        + ansi["WARNING"] + message + ansi["RESET"])


def error(message):
  print(ansi["BOLD"] + ansi["ERROR"] + "[^] " + ansi["RESET"]
        + ansi["ERROR"] + message + ansi["RESET"])


def info(message):
  print(ansi["BOLD"] + ansi["INFO"] + "[+] " + ansi["RESET"]
        + ansi["INFO"] + message + ansi["RESET"])