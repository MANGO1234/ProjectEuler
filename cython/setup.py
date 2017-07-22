from distutils.core import setup
from Cython.Build import cythonize

setup(
    name='cyphon project euler',
    ext_modules=cythonize("euler_cython.pyx"),
)
