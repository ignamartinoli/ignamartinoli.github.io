import { queryCollection } from '@nuxt/content/server'

const escape = (value = '') =>
  value.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')

export default defineEventHandler(async (event) => {
  const { url, name, description } = getSiteConfig(event)
  const posts = await queryCollection(event, 'blog').order('pubDate', 'DESC').all()

  const items = posts.map(post => `    <item>
      <title>${escape(post.title)}</title>
      <description>${escape(post.description)}</description>
      <link>${url}${post.path}</link>
      <guid>${url}${post.path}</guid>
      <pubDate>${new Date(post.pubDate).toUTCString()}</pubDate>
${(post.tags ?? []).map(tag => `      <category>${escape(tag)}</category>`).join('\n')}
    </item>`).join('\n')

  setHeader(event, 'content-type', 'application/rss+xml; charset=utf-8')

  return `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>${escape(name)}</title>
    <description>${escape(description)}</description>
    <link>${url}</link>
${items}
  </channel>
</rss>`
})
