<script setup lang="ts">
const shapes = [
  { position: 'size-20 top-[20%] left-[15%]', shape: '[clip-path:polygon(50%_0%,0%_100%,100%_100%)]', delay: '-3s' },
  { position: 'size-[70px] top-[40%] left-[80%]', shape: 'rounded-[10px] rotate-45', delay: '-12s' },
  { position: 'w-30 h-15 top-[65%] right-[15%]', shape: 'rounded-full', delay: '-8s' },
  { position: 'size-[90px] bottom-[15%] left-[10%]', shape: 'rounded-full', delay: '-20s' }
]

const orbs = [
  'size-112 -top-50 -left-50 from-ctp-mauve to-ctp-pink',
  'size-88 top-1/2 -right-38 from-ctp-blue to-ctp-sapphire [animation-delay:-8s]',
  'size-125 -bottom-63 left-1/5 from-ctp-teal to-ctp-green [animation-delay:-15s]'
]

const PUSH_STRENGTH = 50
const RADIUS = 150

const container = useTemplateRef<HTMLElement>('container')

function onMouseMove(event: MouseEvent) {
  container.value?.querySelectorAll<HTMLElement>('[data-shape]').forEach((el) => {
    const { left, top, width, height } = el.getBoundingClientRect()
    const dx = event.clientX - (left + width / 2)
    const dy = event.clientY - (top + height / 2)
    const distance = Math.hypot(dx, dy)

    if (distance >= RADIUS) {
      el.style.transform = ''
      return
    }

    const factor = (1 - distance / RADIUS) * PUSH_STRENGTH
    const angle = Math.atan2(dy, dx)
    el.style.transform = `translate(${-Math.cos(angle) * factor}px, ${-Math.sin(angle) * factor}px)`
  })
}

onMounted(() => window.addEventListener('mousemove', onMouseMove))
onBeforeUnmount(() => window.removeEventListener('mousemove', onMouseMove))
</script>

<template>
  <div
    ref="container"
    aria-hidden="true"
    class="fixed inset-0 -z-10 h-dvh overflow-hidden bg-linear-135 from-ctp-base via-ctp-mantle to-ctp-crust"
  >
    <div
      v-for="(orb, index) in orbs"
      :key="index"
      class="absolute rounded-full bg-radial opacity-25 blur-[80px] animate-float-orb"
      :class="orb"
    />

    <div
      v-for="(shape, index) in shapes"
      :key="index"
      class="absolute animate-float-shape"
      :class="shape.position"
      :style="{ animationDelay: shape.delay }"
    >
      <div
        data-shape
        class="size-full border border-white/10 bg-white/5 backdrop-blur-md transition-transform duration-400 ease-out"
        :class="shape.shape"
      />
    </div>
  </div>
</template>
