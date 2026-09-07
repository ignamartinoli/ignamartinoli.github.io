<script setup lang="ts">
import type { TocLink } from '@nuxt/content'

const { links } = defineProps<{ links?: TocLink[] }>()

const open = ref(false)
const route = useRoute()

// Picking a section pushes a hash; close the sheet so the scroll is visible.
watch(() => route.hash, () => { open.value = false })
</script>

<template>
  <UDrawer
    v-model:open="open"
    title="Sections"
    :modal="false"
    :overlay="false"
    class="lg:hidden"
  >
    <UButton
      icon="i-lucide-list"
      color="neutral"
      variant="subtle"
      size="xl"
      aria-label="Open the table of contents"
      class="glass fixed end-4 bottom-4 z-50 rounded-full p-3 shadow-lg"
    />

    <template #body>
      <UContentToc
        :links="links"
        highlight
        default-open
        :ui="{
          root: 'static mx-0 px-0 max-h-[60vh] bg-transparent backdrop-blur-none',
          container: 'py-0 border-0',
          trigger: 'hidden'
        }"
      />
    </template>
  </UDrawer>
</template>
