<script setup lang="ts">
import UiProsePre from '@nuxt/ui/components/prose/Pre.vue'

defineOptions({ inheritAttrs: false })
defineProps<{ code?: string, language?: string }>()
</script>

<template>
  <Mermaid v-if="language === 'mermaid'" :code="code ?? ''" />
  <UiProsePre v-else v-bind="$attrs" :code="code" :language="language">
    <slot />
  </UiProsePre>
</template>
