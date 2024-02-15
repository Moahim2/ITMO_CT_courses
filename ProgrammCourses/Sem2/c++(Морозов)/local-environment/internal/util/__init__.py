# Check that all our requirements are satisfied and we can successfully run
from .env import requirements
from .requirements import assert_requirements

assert_requirements(env.requirements)

# Continue running the app
from .env import *
from .process import run_with_stdout
from .template import format_template, format_template_file
from .git import assert_dependencies, dependencies_need_updates
from .docker import run_docker
from .log import info, warning, error
