<script setup lang="ts">
const route = useRoute()

const { data: post } = await useAsyncData(`post-${route.path}`, () =>
  queryCollection('blog').path(route.path).first()
)

if (!post.value) {
  throw createError({ statusCode: 404, statusMessage: 'Post not found', fatal: true })
}

const { data: surround } = await useAsyncData(`surround-${route.path}`, () =>
  queryCollectionItemSurroundings('blog', route.path, { fields: ['description'] })
)

useSeoMeta({
  title: post.value.title,
  description: post.value.description,
  ogImage: absoluteUrl(post.value.heroImage)
})
</script>

<template>
  <UContainer v-if="post">
    <UPage>
      <UPageHeader :title="post.title" :description="post.description">
        <template #headline>
          <PostDate :date="post.pubDate" :updated="post.updatedDate" />

          <PostTags link :tags="post.tags" />
        </template>

        <img
          v-if="post.heroImage"
          :src="post.heroImage"
          alt=""
          width="1020"
          height="510"
          class="mt-8 w-full rounded-xl object-cover"
        >
      </UPageHeader>

      <UPageBody>
        <ContentRenderer :value="post" />

        <UContentSurround :surround="surround" />

        <PostComments />
      </UPageBody>

      <template #right>
        <!-- The root is the grid column itself, so it stretches to the article's
             height; self-start shrinks it back to its own content. -->
        <UContentToc
          :links="post.body.toc?.links"
          highlight
          class="max-lg:hidden lg:glass lg:rounded-lg lg:border lg:self-start lg:top-[calc(var(--ui-header-height)+3rem)] lg:max-h-[calc(100vh-var(--ui-header-height)-6rem)]"
        />
      </template>
    </UPage>

    <PostTocDrawer :links="post.body.toc?.links" />
  </UContainer>
</template>
