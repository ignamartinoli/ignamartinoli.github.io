<script setup lang="ts">
import UiProseImg from '@nuxt/ui/components/prose/Img.vue'

defineOptions({ inheritAttrs: false })
</script>

<template>
  <UiProseImg v-bind="$attrs" loading="lazy" decoding="async" />
</template>
