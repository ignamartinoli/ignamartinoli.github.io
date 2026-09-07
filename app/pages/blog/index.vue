<script setup lang="ts">
const description = 'Thoughts on programming, languages and design.'

const route = useRoute()

const { data: posts } = await useAsyncData('blog-posts', () =>
  queryCollection('blog').order('pubDate', 'DESC').all()
)

const activeTag = computed(() => route.query.tag as string | undefined)

// Only tags actually in use, so the row never offers an empty filter.
const tags = computed(() => [...new Set(posts.value?.flatMap(post => post.tags ?? []))].sort())

const filtered = computed(() => activeTag.value
  ? posts.value?.filter(post => post.tags?.includes(activeTag.value!))
  : posts.value)

useSeoMeta({ title: 'Blog', description })
</script>

<template>
  <UContainer class="py-12">
    <UPageHeader title="Blog" :description="description" />

    <div v-if="tags.length" class="mt-8 flex flex-wrap items-center gap-1.5">
      <UButton
        to="/blog"
        label="All"
        size="xs"
        :color="activeTag ? 'neutral' : 'primary'"
        :variant="activeTag ? 'subtle' : 'solid'"
      />

      <PostTags link :tags="tags" :active="activeTag" />
    </div>

    <UBlogPosts class="mt-12">
      <UBlogPost
        v-for="(post, index) in filtered"
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
      >
        <template #badge>
          <PostTags :tags="post.tags" />
        </template>
      </UBlogPost>
    </UBlogPosts>

    <UEmpty
      v-if="!filtered?.length"
      class="mt-12"
      icon="i-lucide-tag"
      :title="`No posts tagged “${activeTag}”`"
      description="Pick another tag, or clear the filter."
    />
  </UContainer>
</template>
