from distutils.core import setup, Extension

setup(name        = "aff",
      version     = "0.0",
      ext_modules = [Extension("aff",
                               ["aff.c"],
                               include_dirs = ["../lib"],
                               library_dirs = ["../lib"],
                               libraries    = ["lhpc-aff"])])

