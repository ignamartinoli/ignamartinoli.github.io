export default defineAppConfig({
  repo: 'https://github.com/ignamartinoli/ignamartinoli.github.io',
  links: {
    linkedin: 'https://www.linkedin.com/in/ignamartinoli',
    instagram: 'https://instagram.com/tanomartinoli',
    email: 'ignamartinoli@proton.me'
  },
  // Comments. `categoryId` comes from https://giscus.app once Discussions is enabled
  // and the giscus app is installed; the section stays hidden until it is filled in.
  giscus: {
    repoId: 'R_kgDOMB_Ftg',
    category: 'Announcements',
    categoryId: 'DIC_kwDOMB_Fts4DFCiB'
  },
  ui: {
    // Nuxt UI builds its 50-950 scales from a Tailwind colour, and Catppuccin is not one;
    // `violet` is the nearest. The accent you actually see is `--ui-primary` in main.css.
    colors: { primary: 'violet', neutral: 'slate' },
    // Nuxt UI anchors card images to the top, which halves a square hero. Crop from
    // the centre instead so every image still fills the card.
    blogPost: {
      slots: {
        root: 'glass duration-500 hover:scale-[1.02]',
        image: 'object-cover object-center'
      }
    },
    contentToc: {
      variants: {
        active: {
          false: { link: 'text-default hover:text-highlighted transition-colors' }
        }
      }
    },
    // Every article body is a UPageBody, so the prose type scale is set there once
    // (`post-body` in main.css) rather than per page.
    pageBody: { base: 'post-body' },
    // Default thead is bg-muted, a shade off the table body; bg-elevated is the same
    // step up the surface scale and actually reads as a header row.
    prose: {
      thead: { base: 'bg-elevated' }
    }
  }
})
