<script setup lang="ts">
const background = useBackground()
const palette = useCatppuccin()
const colorMode = useColorMode()

// TEMPORARY (background comparison): each background is lazily imported, so only the
// selected one is fetched. Remove this and the bg/ folder once one is chosen.
const modules = import.meta.glob('./bg/*.vue')
const root = useTemplateRef<HTMLElement>('root')

const dark = computed(() => colorMode.value === 'macchiato')

/** Iridescence's hard cosine banding is what reads as metallic; a blur softens it. */
const effect = computed(() => ({
  Iridescence: 'blur-[10px] saturate-75'
}[background.value] ?? ''))

/**
 * Aurora's shader multiplies its ramp by an intensity below 1, so it can only ever
 * darken. On Latte that is a grey veil over the text whatever colours it is given, so
 * it is dark-only and falls back to the original background in light.
 */
const unavailable = computed(() => background.value === 'Aurora' && !dark.value)

const component = computed(() => {
  const loader = unavailable.value ? undefined : modules[`./bg/${background.value}.vue`]
  return loader ? defineAsyncComponent(loader as never) : null
})

const channels = (hex: string) => {
  const n = Number.parseInt(hex.slice(1), 16)
  return [((n >> 16) & 255) / 255, ((n >> 8) & 255) / 255, (n & 255) / 255] as [number, number, number]
}

const toHex = (c: number[]) =>
  `#${c.map(v => Math.round(v * 255).toString(16).padStart(2, '0')).join('')}`

/** Toward black, for shaders that paint opaquely and would otherwise glare. */
const shade = (hex: string, factor: number) => toHex(channels(hex).map(c => c * factor))

/** Toward white, so a light flavour gets a pastel rather than a saturated wash. */
const tint = (hex: string, amount: number) => toHex(channels(hex).map(c => c + (1 - c) * amount))

const rgba = (hex: string, alpha: number) => {
  const [r, g, b] = channels(hex).map(c => Math.round(c * 255))
  return `rgba(${r}, ${g}, ${b}, ${alpha})`
}

const colors = computed<Record<string, unknown>>(() => {
  const p = palette.value
  if (!p.mauve) return {}

  // Tinting far enough for text contrast turned the accent grey, so keep it saturated
  // and let the opaque glass panels carry legibility instead.
  const accent = dark.value ? p.mauve : tint(p.mauve, 0.3)

  return {
    // Aurora's own defaults are [page, accent, page] so the ribbon fades out at the
    // edges. Three accents made it a solid band that competes with the text.
    Aurora: { colorStops: [p.base, p.mauve, p.base], amplitude: 1 },
    Plasma: { color: accent, opacity: dark.value ? 0.8 : 0.35 },
    // Light needs far less contrast, or the grid reads through the glass panels.
    ShapeGrid: {
      borderColor: rgba(p.overlay0, dark.value ? 0.55 : 0.3),
      hoverFillColor: rgba(p.mauve, dark.value ? 0.8 : 0.45),
      // The vignette is what was smothering the corners; fade to the page colour.
      vignetteColor: rgba(p.base, dark.value ? 0.9 : 0.75)
    },
    // Its dots are filled with this gradient, so light needs more alpha, not less.
    DotField: {
      glowRadius: 0, // kills the halo that tracked the cursor
      glowColor: p.mauve,
      dotRadius: 2.2,
      gradientFrom: rgba(p.mauve, dark.value ? 0.7 : 0.85),
      gradientTo: rgba(p.teal, dark.value ? 0.55 : 0.7)
    },
    // Iridescence paints opaque, so it has to carry the light/dark itself.
    Iridescence: { color: channels(dark.value ? shade(p.mauve, 0.28) : tint(p.mauve, 0.88)) }
  }[background.value] ?? {}
})

// The background sits at -z-10 and is pointer-events:none, so the browser never
// hit-tests it — which also stops it firing mouseleave every time the cursor crosses
// page content. Aurora and DotField listen on window; the rest listen on their own
// element, so forward the cursor in.
let stop: (() => void) | undefined

onMounted(() => {
  const forward = (event: MouseEvent) => {
    const el = root.value?.querySelector('canvas') ?? root.value?.firstElementChild
    el?.dispatchEvent(new MouseEvent('mousemove', {
      clientX: event.clientX,
      clientY: event.clientY,
      bubbles: true
    }))
  }

  window.addEventListener('mousemove', forward, { passive: true })
  stop = () => window.removeEventListener('mousemove', forward)
})

onBeforeUnmount(() => stop?.())
</script>

<template>
  <AppBackground v-if="!component" />

  <ClientOnly v-else>
    <div ref="root" class="pointer-events-none fixed inset-0 -z-10" :class="effect">
      <!-- keyed on the flavour: not every component watches its colour props -->
      <component :is="component" :key="JSON.stringify(colors)" v-bind="colors" />
    </div>
  </ClientOnly>
</template>
