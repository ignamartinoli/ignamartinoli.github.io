<script setup lang="ts">
// TEMPORARY (background comparison): dropped entirely from production builds.
const BackgroundSwitcher = import.meta.dev
  ? defineAsyncComponent(() => import('~/components/BackgroundSwitcher.vue'))
  : null
</script>

<template>
  <SiteBackground />
  <component :is="BackgroundSwitcher" v-if="BackgroundSwitcher" />

  <UButton
    to="#main"
    label="Skip to content"
    color="neutral"
    variant="subtle"
    class="sr-only focus:not-sr-only focus:fixed focus:start-4 focus:top-4 focus:z-100"
  />

  <AppHeader />

  <UMain id="main">
    <slot />
  </UMain>

  <AppFooter />
</template>
