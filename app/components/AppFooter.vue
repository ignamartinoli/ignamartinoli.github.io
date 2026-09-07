<script setup lang="ts">
const site = useSiteConfig()
const { repo, links } = useAppConfig()
</script>

<template>
  <UFooter :ui="{ root: 'glass border-y' }">
    <template #left>
      <p class="text-sm text-muted">
        © {{ new Date().getFullYear() }} {{ site.name }}
      </p>
    </template>

    <template #right>
      <UButton
        :to="repo"
        target="_blank"
        icon="i-simple-icons-github"
        color="neutral"
        variant="ghost"
        aria-label="GitHub"
      />
      <UButton
        :to="links.linkedin"
        target="_blank"
        icon="i-simple-icons-linkedin"
        color="neutral"
        variant="ghost"
        aria-label="LinkedIn"
      />
      <UButton
        :to="links.instagram"
        target="_blank"
        icon="i-simple-icons-instagram"
        color="neutral"
        variant="ghost"
        aria-label="Instagram"
      />
      <UButton
        :to="`mailto:${links.email}`"
        icon="i-lucide-mail"
        color="neutral"
        variant="ghost"
        aria-label="Email"
      />
      <UButton
        to="/rss.xml"
        external
        icon="i-lucide-rss"
        color="neutral"
        variant="ghost"
        aria-label="RSS feed"
      />
    </template>
  </UFooter>
</template>
