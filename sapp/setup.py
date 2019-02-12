from setuptools import setup


with open("requirements.txt") as f:
    requirements = f.read().splitlines()

setup(
    name="sapp",
    version="0.1",
    install_requires=requirements,
    packages=["sapp"],
    scripts=["sapp_cli.py"],
    url="https://pyre-check.org/",
    maintainer="Jerry Liu, Zachary Landau, Dominik Gabi",
    maintainer_email="jerryliu55@fb.com, zacharyl@fb.com, dominik@fb.com",
)
