<script setup lang="ts">
const { tags, link, active } = defineProps<{
  tags?: string[]
  /** Render as links to the filtered list. Off inside a UBlogPost, whose overlay link swallows clicks. */
  link?: boolean
  active?: string
}>()
</script>

<template>
  <div class="flex flex-wrap items-center gap-1.5">
    <template v-for="tag in tags" :key="tag">
      <UButton
        v-if="link"
        :to="{ path: '/blog', query: { tag } }"
        :label="tag"
        size="xs"
        :color="tag === active ? 'primary' : 'neutral'"
        :variant="tag === active ? 'solid' : 'subtle'"
      />
      <UBadge v-else :label="tag" size="sm" color="neutral" variant="subtle" />
    </template>
  </div>
</template>
