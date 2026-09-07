<script setup lang="ts">
const { date, updated } = defineProps<{ date: string | Date, updated?: string | Date }>()

const format = (value: string | Date) =>
  new Date(value).toLocaleDateString('en-us', { year: 'numeric', month: 'short', day: 'numeric', timeZone: 'UTC' })
</script>

<template>
  <time :datetime="new Date(date).toISOString()">{{ format(date) }}</time>
  <em v-if="updated" class="ml-2">Last updated on {{ format(updated) }}</em>
</template>
