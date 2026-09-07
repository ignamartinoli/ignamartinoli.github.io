<script setup lang="ts">
// TEMPORARY (background comparison): shipped so the picker works on the deployed site.
// Lazily imported so it stays in its own chunk. Remove once a background is chosen.
const BackgroundSwitcher = defineAsyncComponent(() => import('~/components/BackgroundSwitcher.vue'))
</script>

<template>
  <SiteBackground />
  <component :is="BackgroundSwitcher" />

  <UButton
    to="#main"
    label="Skip to content"
    color="neutral"
    variant="subtle"
    class="sr-only focus:not-sr-only focus:fixed focus:start-4 focus:top-4 focus:z-100"
  />

  <AppHeader />

  <UMain id="main" class="py-12">
    <slot />
  </UMain>

  <AppFooter />
</template>
