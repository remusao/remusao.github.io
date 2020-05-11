---
title: AWS NVMe storage optimized instances
date: 2017-12-27
logo: aws
lang: en
---

I recently had the need for very fast storage on an Amazon AWS machine. I gave a
try to their "NVMe Storage Optimized" instances. I was a bit surprised though
that the drive was not accessible straight away. To make it work, you need to
format and mount it manually.

Formatting the drive found at `/dev/nvme0n1`:
```sh
$ sudo mkfs -t ext4 /dev/nvme0n1
```

Mounting to `/ebs/`:
```sh
$ sudo mkdir /ebs
$ sudo mount -o rw /dev/nvme0n1 /ebs/
$ cd /ebs/
```
