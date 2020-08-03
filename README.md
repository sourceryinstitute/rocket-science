rocket-science
==============

A mini-app for simulating solid rocket motors.

Download and Build
------------------
If you have two-factor authentication set up, download the repository
```bash
git clone git@github.com:sourceryinstitute/rocket-science
```
Otherwise, use
```bash
git clone https://github.com/sourceryinstitute/rocket-science
```
then build with 
```bash
mkdir -p rocket-science/build
cd rocket-science/build
cmake ..
make
ctest
```
