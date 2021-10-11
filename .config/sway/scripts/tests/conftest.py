import hy
from _pytest.python import Module


def pytest_collect_file(path, parent):
    if path.ext == ".hy" and "_test" in path.basename:
        return Module.from_parent(parent,fspath=path)
