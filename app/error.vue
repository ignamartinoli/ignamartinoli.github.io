<script setup lang="ts">
import type { NuxtError } from '#app'

const { error } = defineProps<{ error: NuxtError }>()

useSeoMeta({ title: `${error.statusCode}` })
</script>

<template>
  <UApp>
    <AppBackground />
    <AppHeader />

    <UMain>
      <UError :error="error" />
    </UMain>

    <AppFooter />
  </UApp>
</template>
