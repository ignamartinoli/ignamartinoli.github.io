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
      slots: { image: 'object-cover object-center' }
    },
    contentToc: {
      variants: {
        active: {
          false: { link: 'text-default hover:text-highlighted transition-colors' }
        }
      }
    },
    prose: {
      table: {
        slots: {
          root: 'glass my-6 rounded-xl',
          base: 'rounded-xl'
        }
      },
      thead: {
        base: 'bg-elevated/60 bg-linear-to-b from-white/10 to-transparent'
      },
      th: {
        base: 'py-3.5 tracking-wide'
      },
      tr: {
        base: '[&:first-child>th:first-child]:rounded-ss-xl [&:first-child>th:last-child]:rounded-se-xl [&:last-child>td:first-child]:rounded-es-xl [&:last-child>td:last-child]:rounded-ee-xl'
      }
    }
  }
})
