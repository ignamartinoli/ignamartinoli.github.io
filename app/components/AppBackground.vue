<script setup lang="ts">
const orbs = [
  'size-112 -top-50 -left-50 from-ctp-mauve to-ctp-pink',
  'size-88 top-1/2 -right-38 from-ctp-blue to-ctp-sapphire [animation-delay:-8s]',
  'size-125 -bottom-63 left-1/5 from-ctp-teal to-ctp-green [animation-delay:-15s]'
]

const shapes = [
  'size-20 top-[20%] left-[15%] [clip-path:polygon(50%_0%,0%_100%,100%_100%)] [animation-delay:-3s]',
  'size-[70px] top-[40%] left-[80%] rounded-[10px] rotate-45 [animation-delay:-12s]',
  'w-30 h-15 top-[65%] right-[15%] rounded-full [animation-delay:-8s]',
  'size-[90px] bottom-[15%] left-[10%] rounded-full [animation-delay:-20s]'
]

const PUSH = 50
const RADIUS = 150

const container = useTemplateRef<HTMLElement>('container')
let stop: (() => void) | undefined

onMounted(() => {
  if (!matchMedia('(pointer: fine)').matches || matchMedia('(prefers-reduced-motion: reduce)').matches) return

  const els = [...container.value!.querySelectorAll<HTMLElement>('[data-shape]')]
  let latest!: MouseEvent
  let frame = 0

  // Measuring every shape forces layout, so do it once per frame rather than per event.
  // `translate` is its own property, so the push composes with the keyframes' `transform`.
  const push = () => {
    frame = 0
    for (const el of els) {
      const { left, top, width, height } = el.getBoundingClientRect()
      const dx = latest.clientX - (left + width / 2)
      const dy = latest.clientY - (top + height / 2)
      const distance = Math.hypot(dx, dy)
      const factor = distance < RADIUS ? (1 - distance / RADIUS) * PUSH : 0
      const angle = Math.atan2(dy, dx)
      el.style.translate = factor ? `${-Math.cos(angle) * factor}px ${-Math.sin(angle) * factor}px` : ''
    }
  }

  const onMove = (event: MouseEvent) => {
    latest = event
    frame ||= requestAnimationFrame(push)
  }

  window.addEventListener('mousemove', onMove, { passive: true })
  stop = () => {
    window.removeEventListener('mousemove', onMove)
    cancelAnimationFrame(frame)
  }
})

onBeforeUnmount(() => stop?.())
</script>

<template>
  <div ref="container" aria-hidden="true" class="fixed inset-0 -z-10 h-dvh overflow-hidden">
    <div
      v-for="(orb, index) in orbs"
      :key="index"
      class="absolute rounded-full bg-radial opacity-25 blur-[80px] motion-safe:animate-float-orb"
      :class="orb"
    />

    <div
      v-for="(shape, index) in shapes"
      :key="index"
      data-shape
      class="absolute border border-white/10 bg-white/5 backdrop-blur-md transition-[translate] duration-400 ease-out motion-safe:animate-float-shape"
      :class="shape"
    />
  </div>
</template>
