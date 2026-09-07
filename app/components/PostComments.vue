<script setup lang="ts">
import Giscus from '@giscus/vue'

const { repo, giscus } = useAppConfig()
const colorMode = useColorMode()

const slug = new URL(repo).pathname.slice(1)
const theme = computed(() => colorMode.value === 'macchiato' ? 'catppuccin_macchiato' : 'catppuccin_latte')
</script>

<template>
  <section v-if="giscus.categoryId" class="mt-16">
    <h2 class="mb-6 text-2xl font-bold text-highlighted">
      Comments
    </h2>

    <ClientOnly>
      <Giscus
        :repo="slug"
        :repo-id="giscus.repoId"
        :category="giscus.category"
        :category-id="giscus.categoryId"
        :theme="theme"
        mapping="pathname"
        strict="1"
        reactions-enabled="1"
        emit-metadata="0"
        input-position="top"
        lang="en"
        loading="lazy"
      />
    </ClientOnly>
  </section>
</template>
