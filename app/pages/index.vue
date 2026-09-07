<script setup lang="ts">
const { data: page } = await useAsyncData('home', () =>
  queryCollection('pages').path('/').first()
)

const { data: posts } = await useAsyncData('home-posts', () =>
  queryCollection('blog').order('pubDate', 'DESC').limit(3).all()
)

useSeoMeta({
  title: page.value?.title,
  description: page.value?.description
})
</script>

<template>
  <UContainer>
    <UPageBody v-if="page">
      <ContentRenderer :value="page" />
    </UPageBody>

    <section class="mt-16">
      <div class="mb-6 flex items-end justify-between gap-4">
        <h2 class="text-2xl font-bold text-highlighted">
          Latest posts
        </h2>

        <UButton to="/blog" label="All posts" trailing-icon="i-lucide-arrow-right" variant="link" />
      </div>

      <UBlogPosts>
        <UBlogPost
          v-for="post in posts"
          :key="post.path"
          :to="post.path"
          :title="post.title"
          :description="post.description"
          :image="post.heroImage"
          :date="post.pubDate"
          variant="subtle"
        />
      </UBlogPosts>
    </section>
  </UContainer>
</template>
