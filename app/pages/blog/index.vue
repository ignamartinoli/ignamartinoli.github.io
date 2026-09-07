<script setup lang="ts">
const description = 'Thoughts on programming, languages and design.'

const { data: posts } = await useAsyncData('blog-posts', () =>
  queryCollection('blog').order('pubDate', 'DESC').all()
)

useSeoMeta({ title: 'Blog', description })
</script>

<template>
  <UContainer class="py-12">
    <UPageHeader title="Blog" :description="description" />

    <UBlogPosts class="mt-12">
      <UBlogPost
        v-for="(post, index) in posts"
        :key="post.path"
        :to="post.path"
        :title="post.title"
        :description="post.description"
        :image="post.heroImage"
        :date="post.pubDate"
        :orientation="index === 0 ? 'horizontal' : 'vertical'"
        variant="subtle"
        class="glass transition-transform duration-500 hover:scale-[1.02]"
        :class="index === 0 && 'sm:col-span-2 lg:col-span-3'"
      />
    </UBlogPosts>
  </UContainer>
</template>
