# TODO

## Features

- write /about
- add github issue based comments
- add tags
- add i18n
- add links and social media
- add post invariants db, cqrs
- add post for reflective programming
- finish bqn post
- add post for prolog scheduler
- change kitty to ghostty in post
- improve background
- improve header and footer with overscroll

## Fixes

- organize assets (`public/` is still flat, hero images live in `public/images/`)
- add a `bqn` grammar for code blocks (not shipped by Shiki)
- improve interactivity of the background
- shrink the post GIFs (yazi 8 MB, lazydocker 5.4 MB, zellij 4.8 MB) or convert them to
  <video>; they are ~90% of the site's weight and @nuxt/image cannot optimise GIFs
