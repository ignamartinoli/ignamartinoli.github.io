<script setup lang="ts">
const { code } = defineProps<{ code: string }>()

const colorMode = useColorMode()
const el = useTemplateRef<HTMLElement>('el')
const error = ref<string>()

let id = 0

async function render() {
  if (!el.value) return

  try {
    const { default: mermaid } = await import('mermaid')
    const style = getComputedStyle(document.documentElement)
    const read = (name: string) => style.getPropertyValue(`--catppuccin-color-${name}`).trim() || undefined

    mermaid.initialize({
      startOnLoad: false,
      securityLevel: 'strict',
      fontFamily: 'Inter, sans-serif',
      theme: 'base',
      themeVariables: {
        darkMode: colorMode.value === 'macchiato',
        background: read('base'),
        primaryColor: read('surface0'),
        primaryTextColor: read('text'),
        primaryBorderColor: read('overlay0'),
        lineColor: read('overlay1'),
        secondaryColor: read('surface1'),
        tertiaryColor: read('mantle')
      }
    })

    const { svg } = await mermaid.render(`mermaid-${id++}`, code)
    el.value.innerHTML = svg
    error.value = undefined
  } catch (cause) {
    error.value = String((cause as Error)?.message ?? cause)
  }
}

// `el` is null until ClientOnly mounts its slot, which happens a tick after this
// component's own onMounted — so render off the ref, not off the lifecycle hook.
watch([el, () => colorMode.value], render, { flush: 'post' })
</script>

<template>
  <ClientOnly>
    <div v-show="!error" ref="el" class="my-5 flex justify-center [&_svg]:max-w-full" />

    <div v-if="error" class="my-5">
      <p class="text-sm text-error">
        Diagram failed to render: {{ error }}
      </p>
      <pre class="mt-2 overflow-x-auto rounded-md border border-muted bg-muted px-4 py-3 text-sm">{{ code }}</pre>
    </div>

    <template #fallback>
      <pre class="my-5 overflow-x-auto rounded-md border border-muted bg-muted px-4 py-3 text-sm">{{ code }}</pre>
    </template>
  </ClientOnly>
</template>
