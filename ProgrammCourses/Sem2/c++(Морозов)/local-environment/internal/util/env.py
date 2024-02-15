from os import path, pardir
variables = {"container_repo_path": "/repo"}
requirements = ["requests"]
remote_repo = {"owner": "Kurkin", "repo": "ica-lint-action", "branch": "master"}

dependencies = [{"path": "app/libica-plugin.so",
                 "remote_path": "ica/libica-plugin.so",
                 "repo": remote_repo}]
# transform local paths relative to project workdir
_project_path = path.realpath(
  path.join(
    path.dirname(path.realpath(__file__)),
    path.join(pardir, pardir))
)
for dep in dependencies:
  dep["path"] = path.join(_project_path, dep["path"])