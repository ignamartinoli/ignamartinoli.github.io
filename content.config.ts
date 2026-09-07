import { defineCollection, defineContentConfig, z } from '@nuxt/content'

export default defineContentConfig({
  collections: {
    pages: defineCollection({
      type: 'page',
      source: { include: '*.md' },
      schema: z.object({
        image: z.string().optional()
      })
    }),
    blog: defineCollection({
      type: 'page',
      source: { include: 'blog/**/*.md' },
      schema: z.object({
        pubDate: z.date(),
        updatedDate: z.date().optional(),
        heroImage: z.string().optional()
      })
    })
  }
})
