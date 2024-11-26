---
name: Bug report
about: Create a report to help us improve
title: ''
labels: bug
assignees: ''

---

**Describe and demonstrate the bug**
This should be written as a [ucm transcript](https://www.unison-lang.org/docs/tooling/transcripts/) if possible, calling out the unexpected behavior in the text. e.g.

``` unison :hide
a = 1
```

Here I typo the next command and `ucm` silently does nothing, I would have expected an error message:

``` ucm
scratch/main> add b

```

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Environment (please complete the following information):**
 - `ucm --version` [e.g. "0.5.21", or "1cb2437 (built on 2024-06-03)"]
 - OS/Architecture: [e.g. "macOS 14.5, Intel"]
 - Browser, if applicable: [e.g. "chrome 125.0.6422.142"] (Version numbers are typically found the about menu option)

**Additional context**
Add any other context about the problem here.
