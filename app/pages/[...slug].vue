<script setup lang="ts">
const route = useRoute()

const { data: page } = await useAsyncData(`page-${route.path}`, () =>
  queryCollection('pages').path(route.path).first()
)

if (!page.value) {
  throw createError({ statusCode: 404, statusMessage: 'Page not found', fatal: true })
}

useSeoMeta({
  title: page.value.title,
  description: page.value.description,
  ogImage: absoluteUrl(page.value.image)
})
</script>

<template>
  <UContainer v-if="page" class="py-12">
    <UPageBody>
      <ContentRenderer :value="page" />
    </UPageBody>
  </UContainer>
</template>
