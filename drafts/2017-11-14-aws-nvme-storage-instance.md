---
title: AWS nvme storage optimized instances
date: 2017-11-14
---

Install latest sqlite3:
```sh
sudo apt-get update
sudo apt-get upgrade
sudo apt-get dist-upgrade
sudo add-apt-repository ppa:jonathonf/backports
sudo apt-get update
sudo apt-get install sqlite3 libsqlite3-dev
```


Get recent version of Python 3:
```sh
sudo apt-get install libpython-dev libpython3-dev
sudo apt-get install git
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bash_profile
echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bash_profile
source .pyenv/bin/pyenv 
```

Format `nvme` device:
```sh
sudo mkfs -t ext4 /dev/nvme0n1 
sudo mkdir /ebs
sudo mount -o rw /dev/nvme0n1 /ebs/
cd /ebs/
mkdir data
sudo mkdir data
sudo chown ubuntu data/
sudo chgrp ubuntu data/
cd data/
```
