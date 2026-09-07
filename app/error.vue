<script setup lang="ts">
import type { NuxtError } from '#app'

const { error } = defineProps<{ error: NuxtError }>()

useSeoMeta({ title: `${error.statusCode}` })
</script>

<template>
  <UApp>
    <NuxtLayout>
      <UError :error="error" />
    </NuxtLayout>
  </UApp>
</template>
