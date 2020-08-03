BRIANrocket-science
==============

Welcome to Fortran 2018!

A mini-app for simulating solid rocket motors.

Account Setup
-------------
Execute
```bash
cd 
atom .
```
Then select the `.bashrc` file and add the following lines at the bottom:
```bash
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

export sourcerer_bashrc=/home/sourcerer/.bashrc
if [[ -f "$sourcerer_bashrc" ]]; then
  source "$sourcerer_bashrc"
fi

eval "$(ssh-agent -s)"
ssh-add
```
The select File->Save.

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
