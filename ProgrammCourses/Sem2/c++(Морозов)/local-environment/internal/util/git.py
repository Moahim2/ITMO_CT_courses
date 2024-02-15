import datetime
import requests
import os
import sys
from base64 import b64decode
from .log import error, info

GITAPI = "https://api.github.com/"


def repos_request(repo, path):
  return GITAPI + "repos" + '/'\
         + repo["owner"] + '/' + repo["repo"] + path


def repo_latest_commit(repo):
  """Get latest commit for repository on master branch
  :param: repo: {owner: ..., repo: ...}
  :return: dict representing the commit
  """
  try:
    r = requests.get(repos_request(repo, "/commits/"+repo["branch"]))
    return r.json()
  except Exception as e:
    error("Unable to get repository {} latest commit: {}".format(repo, e))
  return None


def get_tree(url):
  """Get tree from specified url
  :param: tree: link to the tree object on github api
  :return: dict representing the tree"""
  try:
    r = requests.get(url)
    return r.json()
  except Exception as e:
    error("Unable to get tree from url {}: {}".format(url, e))
  return None


def commit_tree(commit):
  """Get the tree from the specified commit
  :param: commit: dict representing commit
  :return: dict representing the tree of the commit
  """
  try:
    tree = commit["commit"]["tree"]["url"]
    return get_tree(tree)
  except Exception as e:
    error("Unable to get commit {} tree: {}".format(commit, e))
  return None


def tree_path(tree, path):
  """Get object url from tree
  :param: tree: dict representing the tree
  :return: url of the object in this tree"""
  try:
    tree = tree["tree"]
    path_object = list(filter(lambda object: object["path"] == path, tree))
    if len(path_object) != 0:
      return path_object[0]["url"]
  except Exception as e:
    error("Unable to find required object {} in tree {}: {}"
          .format(path, tree, e))
  return None


def download_blob(url):
  """Download content blob given its url
  :param: url: link to the content blob on github api
  :return: data of the blob or None on fail"""
  try:
    r = requests.get(url)
    j = r.json()
    contentb64 = j["content"]
    if type(contentb64) != bytes:
      contentb64 = contentb64.encode()
    content = b64decode(contentb64)
    return content
  except Exception as e:
    error("Unable to get content blob of {}: {}".format(url, e))
  return None


def repo_updated_at(repo):
  """Get the repository update time
  :param: repo: {owner: ..., repo: ...}
  :return: datetime object
  """
  try:
    r = requests.get(repos_request(repo, ''))
    j = r.json()
    updated_at = datetime.datetime.strptime(j["updated_at"],
                                            "%Y-%m-%dT%H:%M:%SZ")
    return updated_at
  except Exception as e:
    error("Unable to get repository {} update time: {}".format(repo, e))
  return None


def check_file_update(repo, path):
  """Check if repository has been updated after the file's modification date
  :param: repo: {owner: ..., repo: ...}
  :param: path: local path to file which should be checked
  :return: None on fail, True if local file needs update, False otherwise
  """
  if not os.path.exists(path):
    return True # file doesn't exist -> need to update it
  modification_timestamp = datetime.datetime.fromtimestamp(os.path.getmtime(path))
  repo_update_time = repo_updated_at(repo)
  if repo_update_time is None:
    return None
  return modification_timestamp < repo_update_time


def update_file(repo, remote_path, path):
  """Update local file using specified repository
  :param: repo: {owner: ..., repo: ...}
  :param: remote_path: path to remote file
  :param: path: local path to file which should be updated
  :return: True on successful update, False otherwise
  """
  info("Updating remote dependency: {}".format(os.path.basename(path)))
  # traverse commit tree to find the file
  latest_commit = repo_latest_commit(repo)
  if latest_commit is None:
    return False
  tree = commit_tree(latest_commit)
  if tree is None:
    return False
  split_part = remote_path.split('/')
  for i, path_part in enumerate(split_part):
    tree = tree_path(tree, path_part)
    if tree is None:
      return False
    if i < len(split_part) - 1:
      tree = get_tree(tree)
    if tree is None:
      return False

  # download and save file
  content = download_blob(tree)
  with open(path, "wb") as file:
    file.write(content)

  # change modification date to one specified in latest update
  update_time = repo_updated_at(repo)
  if update_time is None:
    return False
  os.utime(path, (update_time.timestamp(), update_time.timestamp()))
  return True


def assert_dependencies(dependencies):
  """Make sure that all remote dependencies are satisfied, kill script on fail
  :param: dependencies: [{"repo": ..., "path": ..., "remote_path": ...}, ...]"""
  for dep in dependencies:
    # try updating if needed and fail if we can't
    if check_file_update(dep["repo"], dep["path"]) and not update_file(**dep):
      error("Unable to update dependency {}. Quitting.".format(dep))
      sys.exit(1)


def dependencies_need_updates(dependencies):
  for dep in dependencies:
    if check_file_update(dep["repo"], dep["path"]):
      return True
  return False