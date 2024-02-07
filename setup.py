import sys
import pathlib
from setuptools import setup
from cmake_build_extension import BuildExtension, CMakeExtension

this_directory = pathlib.Path(__file__).parent
long_description = (this_directory / "README.md").read_text()
long_description_content_type="text/markdown"

setup(
    name="dioptic.profileparser",
    version="0.2.1",
    author="Peter Würtz",
    author_email="pwuertz@gmail.com",
    url="https://github.com/dioptic/profileparser",
    description="Parser library for ARGOS declarative profiles.",
    long_description=long_description,
    long_description_content_type=long_description_content_type,
    #namespace_packages=["dioptic"],
    ext_modules=[CMakeExtension(
        name="dioptic.pyprofileparser",
        source_dir="python",
        install_prefix=".",
        cmake_configure_options=[
            f"-DPYTHON_EXECUTABLE={pathlib.Path(sys.executable)}",
            f"-DPYTHON3_EXECUTABLE={pathlib.Path(sys.executable)}",
	],
    )],
    cmdclass=dict(build_ext=BuildExtension),
)
