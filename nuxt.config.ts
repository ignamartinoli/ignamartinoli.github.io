const title = 'Tano\'s Blog'

export default defineNuxtConfig({
  modules: ['@nuxt/content', '@nuxt/ui', '@nuxtjs/sitemap'],

  app: {
    head: {
      titleTemplate: `%s · ${title}`,
      link: [
        { rel: 'icon', type: 'image/svg+xml', href: '/favicon.svg' },
        { rel: 'alternate', type: 'application/rss+xml', title, href: '/rss.xml' }
      ]
    }
  },

  colorMode: {
    preference: 'macchiato',
    fallback: 'macchiato',
    classSuffix: ''
  },

  css: ['~/assets/css/main.css', 'katex/dist/katex.min.css'],

  site: {
    url: 'https://ignamartinoli.github.io',
    name: title,
    description: 'Welcome to my website!'
  },

  content: {
    build: {
      markdown: {
        toc: { depth: 3, searchDepth: 3 },
        remarkPlugins: { 'remark-math': {} },
        rehypePlugins: { 'rehype-katex': {} },
        highlight: {
          theme: { light: 'catppuccin-latte', default: 'catppuccin-latte', dark: 'catppuccin-macchiato' },
          langs: ['racket', 'haskell', 'smalltalk', 'prolog', 'python', 'http', 'julia', 'bash', 'json']
        }
      }
    }
  },

  // Smooth-scroll when the ToC pushes a heading hash. Scoped to hash navigation:
  // page changes return no `behavior`, so they still jump instantly.
  router: {
    options: { scrollBehaviorType: 'smooth' }
  },

  // Mermaid lazily imports a module per diagram type. Left to be discovered mid-session,
  // Vite re-runs its dep optimiser and the already-issued chunk URLs 404.
  vite: {
    optimizeDeps: { include: ['mermaid'] }
  },

  nitro: {
    prerender: { routes: ['/rss.xml'] }
  },

  devtools: { enabled: true },
  compatibilityDate: '2024-04-03'
})
