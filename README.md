# Tano's Blog

Personal blog built with [Nuxt](https://nuxt.com), [Nuxt Content](https://content.nuxt.com)
and [Nuxt UI](https://ui.nuxt.com). Statically generated and deployed to GitHub Pages.

## Development

```bash
pnpm install
pnpm dev        # http://localhost:3000
pnpm generate   # static build in .output/public
pnpm preview
```

## Writing

Posts are markdown files in `content/blog/`. Front matter:

```yaml
---
title: 'Post title'
description: 'Shown in listings, RSS and social cards'
pubDate: 'Aug 21 2025'
updatedDate: 'Sep 01 2025'  # optional
heroImage: '/images/post-hero.png'  # optional, lives in public/images/
---
```

`content/index.md` and `content/about.md` are the home and about pages — same
markdown, no front matter beyond `title`/`description`/`image`.

## Theming

The colour mode values are the Catppuccin flavours themselves — `latte` and `macchiato`,
with Macchiato as the default — so the palette comes straight from
`@catppuccin/tailwindcss`. `main.css` re-points Tailwind's `dark:` variant at
`.macchiato` and maps the flavour onto Nuxt UI's `--ui-*` tokens. `system` colour mode is
not available: it resolves to `light`/`dark`, which are not flavour names.

Code blocks are set in a personal Iosevka build. `public/fonts/` holds a latin +
punctuation + box-drawing subset of the four faces, regenerated from
`~/.local/share/fonts/IosevkaCustom/` after a font rebuild with:

```bash
for f in Regular Bold Italic BoldItalic; do
  uvx --from 'fonttools[woff]' pyftsubset ~/.local/share/fonts/IosevkaCustom/IosevkaCustom-$f.ttf \
    --output-file=public/fonts/IosevkaCustom-$f.woff2 --flavor=woff2 \
    --unicodes='U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2122,U+2190-21BB,U+2212,U+2215,U+2500-257F,U+25A0-25FF,U+FEFF,U+FFFD'
done
```

Code blocks are highlighted with Catppuccin (Latte / Macchiato). A language must be
listed in `content.build.markdown.highlight.langs` in `nuxt.config.ts` to be highlighted.
Maths is written in `$…$` / `$$…$$` and rendered with KaTeX.

## Layout

| Path | Purpose |
| --- | --- |
| `nuxt.config.ts` | modules, site URL, markdown pipeline |
| `app.config.ts` | Nuxt UI colours, repo link |
| `content.config.ts` | collection schemas (`pages`, `blog`) |
| `app/assets/css/main.css` | Catppuccin palettes mapped onto Nuxt UI tokens |
| `app/components/` | header, animated background, post date |
| `app/pages/` | `[...slug]` (content pages), `blog/` (index + post) |
| `server/routes/rss.xml.ts` | RSS feed |

## Credits

The alternative page backgrounds in `app/components/bg/` are [Vue Bits](https://vue-bits.dev)
components by David Haz, used under MIT + Commons Clause. Two carry local fixes — see
`app/components/bg/README.md` for the details and `app/components/bg/LICENSE.md` for the
licence. They are development-only and are excluded from production builds.
