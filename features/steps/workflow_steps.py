"""Step definitions for workflow plugin BDD tests."""

import re as _re
from pathlib import Path

from behave import given, use_step_matcher

use_step_matcher("re")


def create_workflow_file(context, name, content):
    """Helper to create a workflow TOML file in .tickets/workflows/."""
    workflows_dir = Path(context.test_dir) / '.tickets' / 'workflows'
    workflows_dir.mkdir(parents=True, exist_ok=True)
    wf_path = workflows_dir / f'{name}.toml'
    wf_path.write_text(content)
    return wf_path


@given(r'a workflow file "(?P<name>[^"]+)" with description "(?P<desc>[^"]+)"')
def step_workflow_file_with_desc(context, name, desc):
    """Create a minimal workflow file with a description."""
    content = f'''workflow = "{name}"
description = "{desc}"
version = 1

[[steps]]
id = "step1"
title = "Step 1"
'''
    create_workflow_file(context, name, content)


@given(r'a workflow file "(?P<name>[^"]+)" with steps')
def step_workflow_file_with_steps(context, name):
    """Create a workflow file with steps from a table."""
    lines = [f'workflow = "{name}"', f'description = "{name} workflow"', 'version = 1', '']

    # Check for variables referenced in titles ({{var}})
    var_names = set()
    for row in context.table:
        for m in _re.finditer(r'\{\{(\w+)\}\}', row['title']):
            var_names.add(m.group(1))

    for var_name in var_names:
        lines.append(f'[vars.{var_name}]')
        lines.append(f'description = "{var_name}"')
        lines.append(f'required = true')
        lines.append('')

    for row in context.table:
        lines.append('[[steps]]')
        lines.append(f'id = "{row["id"]}"')
        lines.append(f'title = "{row["title"]}"')
        if row['needs'].strip():
            needs = ', '.join(f'"{n.strip()}"' for n in row['needs'].split(','))
            lines.append(f'needs = [{needs}]')
        lines.append('')

    create_workflow_file(context, name, '\n'.join(lines))


@given(r'a simple workflow file "(?P<name>[^"]+)" with (?P<count>\d+) steps')
def step_simple_workflow_file(context, name, count):
    """Create a simple workflow file with N steps, no variables."""
    lines = [f'workflow = "{name}"', f'description = "{name} workflow"', 'version = 1', '']
    for i in range(1, int(count) + 1):
        lines.append('[[steps]]')
        lines.append(f'id = "step{i}"')
        lines.append(f'title = "Step {i}"')
        if i > 1:
            lines.append(f'needs = ["step{i-1}"]')
        lines.append('')
    create_workflow_file(context, name, '\n'.join(lines))


@given(r'a workflow file "(?P<name>[^"]+)" with required variable "(?P<var>[^"]+)"')
def step_workflow_with_required_var(context, name, var):
    """Create a workflow with a required variable."""
    content = f'''workflow = "{name}"
description = "{name} workflow"
version = 1

[vars.{var}]
required = true

[[steps]]
id = "step1"
title = "Step with {{{{{var}}}}}"
'''
    create_workflow_file(context, name, content)


@given(r'a workflow file "(?P<name>[^"]+)" with variable "(?P<var>[^"]+)" pattern "(?P<pattern>[^"]+)"')
def step_workflow_with_var_pattern(context, name, var, pattern):
    """Create a workflow with a pattern-validated variable."""
    content = f'''workflow = "{name}"
description = "{name} workflow"
version = 1

[vars.{var}]
required = true
pattern = "{pattern}"

[[steps]]
id = "step1"
title = "Step with {{{{{var}}}}}"
'''
    create_workflow_file(context, name, content)


@given(r'a workflow file "(?P<name>[^"]+)" with variable "(?P<var>[^"]+)" enum "(?P<vals>[^"]+)"')
def step_workflow_with_var_enum(context, name, var, vals):
    """Create a workflow with an enum-validated variable."""
    enum_list = ', '.join(f'"{v.strip()}"' for v in vals.split(','))
    content = f'''workflow = "{name}"
description = "{name} workflow"
version = 1

[vars.{var}]
required = true
enum = [{enum_list}]

[[steps]]
id = "step1"
title = "Step with {{{{{var}}}}}"
'''
    create_workflow_file(context, name, content)


@given(r'a workflow file "(?P<name>[^"]+)" with multiline description and variable "(?P<var>[^"]+)"')
def step_workflow_with_multiline(context, name, var):
    """Create a workflow with a triple-quoted multiline description."""
    content = f'''workflow = "{name}"
description = """
This is a multiline description for {{{{{var}}}}}.
It spans multiple lines.
"""
version = 1

[vars.{var}]
required = true

[[steps]]
id = "step1"
title = "Do something for {{{{{var}}}}}"
'''
    create_workflow_file(context, name, content)


@given(r'a workflow file "(?P<name>[^"]+)" with variable "(?P<var>[^"]+)" default "(?P<default>[^"]+)" in step title "(?P<title>[^"]+)"')
def step_workflow_with_var_default(context, name, var, default, title):
    """Create a workflow with a variable that has a default value."""
    content = f'''workflow = "{name}"
description = "{name} workflow"
version = 1

[vars.{var}]
default = "{default}"

[[steps]]
id = "step1"
title = "{title}"
'''
    create_workflow_file(context, name, content)
