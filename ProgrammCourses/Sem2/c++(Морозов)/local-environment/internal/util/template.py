def format_template(template, variables):
  """Substitute {{variable}}-style string in the template string
  :param template: String with variables which to replace
  :param variables: Variables dictionary in the form variable:value
  :return: The formatted result
  """
  if not isinstance(variables, dict):
    raise ValueError("Template variables are supposed "
                     "to be in the form of a dictionary.")

  for v, i in variables.items():
    template = template.replace("{{" + v + "}}", str(i))

  return template


def format_template_file(input_filepath, output_filepath, variables):
  """Substitute {{variable}}-style strings in the file specified by input_filepath
  with the specified variables, and write the result to output_filepath.
  :param input_filepath: Template path, in which to substitute the variables
  :param output_filepath: Output path with the template variables substituted
  :param variables: Variables dictionary for the template in form variable:value
  """
  if not isinstance(variables, dict):
    raise ValueError("Template variables are supposed "
                     "to be in the form of a dictionary.")

  with open(input_filepath, 'r') as file:
    template = file.read()

  template = format_template(template, variables)

  with open(output_filepath, 'w') as file:
    file.write(template)
