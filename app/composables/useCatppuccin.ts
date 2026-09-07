// TEMPORARY (background comparison): reads the live palette so the Vue Bits
// backgrounds follow the Latte/Macchiato toggle like the rest of the site.
const NAMES = ['mauve', 'teal', 'base', 'overlay0'] as const

export function useCatppuccin() {
  const colorMode = useColorMode()
  const palette = ref<Record<string, string>>({})

  const read = () => {
    const style = getComputedStyle(document.documentElement)
    palette.value = Object.fromEntries(
      NAMES.map(name => [name, style.getPropertyValue(`--catppuccin-color-${name}`).trim()])
    )
  }

  onMounted(read)
  watch(() => colorMode.value, () => nextTick(read))

  return palette
}
