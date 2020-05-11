---
title: Debugging iptables rules
date: 2018-10-24
logo: linux
lang: en
---

A very quick one! I've been playing with `iptables` rules lately and was
wondering: "How do I debug these things?". How can I know if the rule is
matching what I want, or if it's doing something at all? I found *one way* to do
so, and it's probably not the only one (or the best one), but I thought I'd
share it anyway. Feel free to send me your tips and tricks!

Using the `LOG` action and optionally specifying a custom log prefix, it's
relatively easy to see what packets are targeted by a rule. For example:
```sh
iptables -A FORWARD -p tcp -j LOG --log-prefix='[netfilter] '
```

Will then write logs into `/var/log/kern.log`, which you can monitor using `tail`:
```sh
tail -f /var/log/kern.log
```

I found this to be pretty convenient!
