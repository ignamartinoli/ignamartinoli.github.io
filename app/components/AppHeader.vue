<script setup lang="ts">
const site = useSiteConfig()
const colorMode = useColorMode()
const { repo } = useAppConfig()

// `swap` must await the DOM change: colour-mode flips the html class in a Vue watcher,
// so without nextTick the API snapshots the "after" state before the class lands.
function toggleFlavour() {
  const swap = async () => {
    colorMode.preference = colorMode.value === 'macchiato' ? 'latte' : 'macchiato'
    await nextTick()
  }

  document.startViewTransition ? document.startViewTransition(swap) : swap()
}

const links = [
  { label: 'Home', to: '/', icon: 'i-lucide-house', exact: true },
  { label: 'Blog', to: '/blog', icon: 'i-lucide-newspaper' },
  { label: 'About', to: '/about', icon: 'i-lucide-user' }
]
</script>

<template>
  <UHeader :title="site.name" :toggle="false" :ui="{ root: 'glass border-y' }">
    <template #title>
      <img src="/favicon.svg" alt="" width="32" height="32">
      <span>{{ site.name }}</span>
    </template>

    <UNavigationMenu :items="links" />

    <template #right>
      <UButton
        color="neutral"
        variant="ghost"
        aria-label="Toggle Catppuccin flavour"
        @click="toggleFlavour"
      >
        <UIcon name="i-lucide-sun" class="size-5 dark:hidden" />
        <UIcon name="i-lucide-moon" class="size-5 hidden dark:block" />
      </UButton>

      <UButton
        :to="repo"
        target="_blank"
        icon="i-simple-icons-github"
        color="neutral"
        variant="ghost"
        aria-label="GitHub repository"
      />

      <UDropdownMenu :items="links" :content="{ align: 'end' }" class="lg:hidden">
        <UButton icon="i-lucide-menu" color="neutral" variant="ghost" aria-label="Open menu" />
      </UDropdownMenu>
    </template>
  </UHeader>
</template>
