# Vendored backgrounds

Vue Bits components, copied in with its `jsrepo` CLI (that project publishes no npm
package — copying the source is its only distribution model).

Copyright (c) 2025 David Haz. MIT + Commons Clause — see `LICENSE.md` in this folder.
Source: https://vue-bits.dev

## Local modifications

Two files carry fixes, each marked with a `LOCAL EDIT` comment:

- `DotField.vue` — offsets were captured once as `rect.left + window.scrollX` and
  compared against `pageX`, so the cursor drifted by the scroll distance. Now uses
  `clientX`/`clientY`, which are viewport-relative like the rect.
- `ShapeGrid.vue` — the vignette was hardcoded to `#14110E`, a dark wash that smothers
  the corners on a light theme. It is now a `vignetteColor` prop defaulting to the
  original value.

`Aurora.vue`, `Iridescence.vue` and `Plasma.vue` are unmodified.

These are wired up by `app/components/SiteBackground.vue`, which is dev-only.
