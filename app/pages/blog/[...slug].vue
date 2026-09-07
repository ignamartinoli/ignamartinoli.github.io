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
  <UContainer v-if="post" class="py-12">
    <UPage>
      <UPageHeader :title="post.title" :description="post.description">
        <template #headline>
          <PostDate :date="post.pubDate" :updated="post.updatedDate" />
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
        <UContentToc
          :links="post.body.toc?.links"
          highlight
          class="max-lg:hidden lg:glass lg:rounded-xl"
        />
      </template>
    </UPage>

    <PostTocDrawer :links="post.body.toc?.links" />
  </UContainer>
</template>
