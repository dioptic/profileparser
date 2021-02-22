from setuptools import find_packages, setup
from cmake_build_extension import BuildExtension, CMakeExtension

setup(
    name="dioptic.pyprofileparser",
    version="0.1",
    author="Peter Würtz",
    description="Parser library for ARGOS declarative profiles.",
    namespace_packages=["dioptic"],
    ext_modules=[CMakeExtension(
        name="dioptic.pyprofileparser",
        source_dir="python",
        install_prefix=".",
    )],
    cmdclass=dict(build_ext=BuildExtension),
)
