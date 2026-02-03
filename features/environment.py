"""Behave environment setup for ticket CLI tests."""

import os
import shutil
import tempfile
from pathlib import Path


def before_all(context):
    """Set up test environment before all tests."""
    # Store the project directory (where the ticket script lives)
    context.project_dir = Path(__file__).parent.parent.resolve()


def before_scenario(context, scenario):
    """Create a fresh temporary directory for each scenario."""
    # Create a temporary directory for this scenario
    context.test_dir = tempfile.mkdtemp(prefix='ticket_test_')

    # Initialize tracking
    context.tickets = {}
    context.last_created_id = None
    context.stdout = ''
    context.stderr = ''
    context.returncode = None


def after_scenario(context, scenario):
    """Clean up temporary directories after each scenario."""
    if hasattr(context, 'test_dir') and os.path.exists(context.test_dir):
        shutil.rmtree(context.test_dir)
    if hasattr(context, 'plugin_dir') and os.path.exists(context.plugin_dir):
        shutil.rmtree(context.plugin_dir)


def before_feature(context, feature):
    """Called before each feature file is processed."""
    pass


def after_feature(context, feature):
    """Called after each feature file is processed."""
    pass
