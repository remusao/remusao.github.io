---
title: Parsing Thunderbird message filtering rules
date: 2019-11-24
logo: thunderbird
lang: en
issue: 73
---

> [Thunderbird](https://www.thunderbird.net/) is a free email application that's
> easy to set up and customize - and it's loaded with great features!

I have been using Thunderbird as my email client of choice for many years, it
is the hub which allows me to manage all the emails I get: work, multiple
personal addresses, etc. As of today my combined mail folders are about `30GB`
and counting. Still, Thunderbird has been running rock solid and I had no issue
so far.

I have to say that I make extensive use of folders, labels and *message
filters*. These features combined allow to effortlessly manage a high volume of
emails by filtering, sorting, tagging, dispatching emails; greatly reducing the
amount of manual work needed to process then read them.

As an example, I receive a lot of emails notifications from GitHub, both for work and personal
OSS projects. I also tend to follow activity of repositories which I find
interesting; but I do not want to have to go through all these notifications
myself. A few years ago I adopted the following strategy:

1. Create folders or subfolders for each repository:
  * `github/work/repo1`,
  * `github/work/repo2`,
  * `github/perso/repo3`,
  * etc.
2. Create a first filter rule *for each repository* to automatically move
   GitHub notifications to their respective folders. This can be achieve by
   detecting `notifications@github.com` using a condition of the form `From is
   notifications@github.com` and adding a `Move Message to` action.
3. Optionally, I create rules to automatically mark as read any
   message which is not directed to me specifically (e.g.: via direct mention
   of my GitHub handler, PR request, issue assigned, etc.); especially on very
   active repositories. This can be achieved by detecting things like
   `mention@noreply.github.com` or `assign@noreply.github.com` in the `Cc`
   field.

GitHub has a [full page of documentation](https://help.github.com/en/github/receiving-notifications-about-activity-on-github/about-email-notifications)
describing all the values you can use to filter email notifications. This is
*really useful*. For example I tag emails where I am directly mentioned in
**red** but notifications about discussions that I was involved in but not
directly mentioned with **blue**. This allows to filter at a glance what I
really need to check, versus what I can check later. In practice, more than 90%
of notifications do not need to be checked manually. This is huge!

Unfortunately, as the number of filters grows, I found the built-in interface
to not be very convenient. Finding or editing rules can become a pain.
Fortunately, Thunderbird stores all the filters for a given account in a
single, plain text file named: `msgFilterRules.dat`, located in your
Thunderbird profile folder. So I decided to write [a
library](https://github.com/remusao/thunderbird-msg-filters) which allows to
manipulate these rules through a minimal and type-safe API; in *TypeScript*.

The goal of the project is to allow developing more high-level tooling to
read, transform, extend, update the rules while not having to worry about the
low-level parsing details. It would also be possible to store the rules in a
nicer format then use the library to produce the final file which can be
consumed by Thunderbird.

To install the library: `npm install --save thunderbird-msg-filters`.

You can then use it to parse a `msgFilterRules.dat` file which you can find in
your Thunderbird folder (one per email account configured):

```typescript
import { parse } from 'thunderbird-msg-filters';

// A typical `msgFilterRules.dat` could contain this raw value.
const rules = parse(`
version="9"
logging="no"
name="rule 1"
enabled="yes"
type="17"
action="Mark flagged"
condition="OR (subject,contains,foo) OR (subject,contains,bar)"
name="rule 2"
enabled="yes"
type="17"
action="Mark read"
action="JunkScore"
actionValue="100"
condition="ALL"
`);
```

You can also format rules to the same original format:

```typescript
import { format } from 'thunderbird-msg-filters';

const rules = format({
  version: '9',
  logging: 'no',
  rules: [
    {
      name: 'rule 1',
      enabled: 'yes',
      type: '17',
      actions: [{ name: 'Mark flagged'}],
      condition: 'OR (subject,contains,foo) OR (subject,contains,bar)'
    },
    {
      name: 'rule 2',
      enabled: 'yes',
      type: '17',
      actions: [
        { name: 'Mark read' }
        { name: 'JunkScore', value: '100' }
      ],
      condition: 'ALL'
    },
  ]
});
```

The library is currently very young and will probably contain bugs, so use at
your own risks. Feel free to use it and [open an issue](https://github.com/remusao/thunderbird-msg-filters/issues/new)
for any feedback!

**What's next?** I plan to use this library to store the message filters rules
in a separate location and synchronize it potentially between multiple
computers. The nice thing is that the rules can now be expressed in a typed
language (TypeScript) which will give me more confidence about their validity.
It is also much easier to manipulate them from a single file, in a familiar
developer environment (i.e.: your editor of choice) than through the
Thunderbird interface.

GitHub repository: https://github.com/remusao/thunderbird-msg-filters/
