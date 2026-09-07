<script setup lang="ts">
const background = useBackground()
const colorMode = useColorMode()

onMounted(() => {
  const saved = localStorage.getItem('background') as Background
  if (BACKGROUNDS.includes(saved)) background.value = saved
})

watch(background, value => localStorage.setItem('background', value))
</script>

<template>
  <ClientOnly>
    <div class="glass fixed bottom-4 start-4 z-50 flex items-center gap-2 rounded-lg p-2">
      <UIcon name="i-lucide-image" class="size-4 text-muted" />
      <USelect v-model="background" :items="[...BACKGROUNDS]" size="xs" class="w-40" />

      <span v-if="background === 'Aurora' && colorMode.value !== 'macchiato'" class="text-xs text-muted">
        dark only
      </span>
    </div>
  </ClientOnly>
</template>
