---
title: People in Cuba Built Their own Internet
date: 2017-12-30
logo: ccc
issue: 9
lang: en
---

Today is the last day of 34c3 and this year was amazing. There is a
lot to talk about, a lot of information to digest, and so many new
ideas to experiment with. I plan on writing a review of the talks I
preferred and outline the take aways in a future post, but for now
I'd like to talk about one presentation in particular: "The Internet in Cuba:
A Story of Community Resilience". You can find the [full video online](https://media.ccc.de/v/34c3-8740-the_internet_in_cuba_a_story_of_community_resilience).
This talk gave me a lot of hope, and I will tell you why, but
first, let's quickly summarize the situation in Cuba, regarding access
to the Internet (most of the information is from the actual talk).

The only way to access the Internet in Cuba is through hotspots accessible in
each city. This is not ideal for several reasons:

1. There are not so many hotspots (~500 in the country, according to a recent
   estimation), which means that you need to go there to get an access and it's
   shared with many people: bandwidth is not great! (maximum 1 MB/s).
2. It's expansive, about 1 CUC/hour (~1 euro per hour).

There are some alternatives, not sanctioned by the government:

1. *El Paquete*, which is basically an external drive you can buy once a week,
   containing data dumped from the internet: TV shows, movies, etc. You can
   check this website to know more about what it contains:
   [paquetedecuba.com](http://paquetedecuba.com) (in *http* only!)
2. *WiFi sharing*, where someone shares his connection to a hotspot with other
   people and make them pay a smaller fee (e.g.: using Connectify).
3. Isolated networks.

This last point is the amazing one. There are multiple isolated networks such as
Universities (and inter-Universities networks), but the most interesting are the
ones called *SNET*, which stands for Street Network. In Havana alone, one such
network connects more than 100k people. And it's not something provided by an
FAI, it has been built from the ground up by people! And multiple other cities
have it too, except they are not connected with each other. And they are not
connected to the Internet either. In fact, this is forbidden, as it would be
illegal.

It's hard to imagine how it got to that scale, but it has not always
been that way. In fact, it all started as a multitude of much smaller
gaming LAN networks used to play multiplayer games. They then realized
that they could connect with each other, across streets, allowing
neighborhoods to play or communicate.

Nowadays the organization is slightly more complex and is composed of different
levels. For more details you can read directly the [paper published at
the Internet Measurement Conference](https://conferences.sigcomm.org/imc/2017/papers/imc17-final186.pdf), November 2017 in London. But to
put it simply, there are local *nodes* at the scale of neighborhoods,
connecting up to 200 people. The connection at this scale is made of a
mix of WiFi connections and Ethernet wires. Nodes are then connected to
regional *pillars*, which in turn are connected to a few other pillars. In
short, *SNET* is literally an Internet on its own.

And what do people do on this SNET? Well, basically the same thing people do on
the Internet:

* They have HTTP websites to serve content.
* Portals to discover more content.
* Gaming (the most popular usage).
* Forums (*WifiNet*, the biggest one, has 56k active users).
* Real time communications.
* Mirrors of Internet websites such as Wikipedia.
* Sub-communities, one of which is mostly working on open-source stuff; they
  setup their own instance of Gitlab and built their own search engine for
  *SNET*.

Of course there are some pain points such as cost of the equipment, lack of
emails, out-dated software, etc. But overall they are doing a fantastic job
developing and maintaining such an infrastructure on their own.

This seriously gives me hope for the future. Because it shows that it
is possible to build alternative networks or internets at a large scale
without multi-billion international companies. Of course it's a lot of
work, but it's possible, with hardware that you can buy online today.

This gives a bit of perspective after net neutrality was killed in the US a few
days ago. The Cuba situation shows that starting from scratch, with no private
support, people were able to invent and build the tools they needed to connect
and communicate with each other.
