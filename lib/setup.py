from setuptools import setup, find_packages, Extension

idle = Extension(name = 'idle',
                 sources = ['idle.c'],
                 extra_link_args = ['-lX11', '-lXss'],
                 )

setup(
    name = "idle",
    version = "0.1",
    packages = find_packages(),
    ext_modules = [idle],
    author = "Chris Newton",
    author_email = "redshodan@gmail.com",
    description = "This is an X11 interface to manage user idle time",
    license = "GPLv3",
    keywords = "x11 remacs",
    url = "http://code.google.com/p/remacs/",    
    )
