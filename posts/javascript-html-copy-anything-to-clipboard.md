---
title: Copy Anything to Clipboard in HTML and Javascript
date: 2018-06-09
logo: html5 
issue: 13
lang: en
---

Not long ago, I was looking for a way to copy the content of any HTML element on
a page to clipboard. What I thought would be an easy task turned out to be more
complicated than expected. In the end I had to use some trick which I will show
here.

The way this will work is as follows: when clicking an element on the page, its
`innerText` will be copied to Clipboard. To achieve this we will need to:

* Attach an `onclick` event listener to our element (or to several elements)
* When a `click` event is received, emit a fake `copy` event

```javascript
  // This variable will be used to store a reference to the
  // latest element clicked.
  let selected = null;

  // Attach our custom copy-to-clipboard-on-click trick to `elt`
  const elt = document.querySelector('...');
  elt.onclick = () => {
    // Store reference to this element in the global `selected`
    selected = elt;
    // Emit fake `copy` event
    document.execCommand('copy');
    // Remove reference from `selected` to allow normal copies
    // to be performed on the page.
    setTimeout(() => { selected = null; }, 1000);
  };
```

* Listen to this `copy` event and copy the content of the latest element
   clicked to clipboard.


```javascript
  // Intercept copy events
  document.addEventListener('copy', (e) => {
    if (selected === null) {
      // No element was clicked
      return;
    }

    // Copy content of clicked element to clipboard
    e.clipboardData.setData(
      'text/plain',
      selected.innerText,
    );

    // We want our data, not data from any selection,
    // to be written to the clipboard
    e.preventDefault();
  });

```
