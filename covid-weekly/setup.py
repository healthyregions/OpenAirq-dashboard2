"""Packaging."""
from setuptools import find_packages, setup


def get_version():
    """Get package version."""
    with open("VERSION") as buffer:
        return buffer.read()


setup(
    name="covid-weekly",
    version=get_version(),
    url="https://github.com/GeoDaCenter/OpenAirq-covid",
    author="Camen Piho",
    author_email="camen.pihor.@gmail.com",
    packages=find_packages(),
    include_package_data=True,
    description="Utilities for retrieving weekly COVID-19 case data by Chicago zipcode",
    long_description="\n" + open("README.md").read(),
)
