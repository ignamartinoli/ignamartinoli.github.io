<!--
  Vue Bits — https://vue-bits.dev
  Copyright (c) 2025 David Haz. MIT + Commons Clause; see ./LICENSE.md.
-->
<script setup lang="ts">
import { Mesh, Program, Renderer, Triangle } from 'ogl';
import { onBeforeUnmount, onMounted, useTemplateRef, watch } from 'vue';

interface PlasmaProps {
  color?: string;
  speed?: number;
  direction?: 'forward' | 'reverse' | 'pingpong';
  scale?: number;
  opacity?: number;
  renderScale?: number;
  maxDpr?: number;
  targetFps?: number;
  iterations?: number;
}

const props = withDefaults(defineProps<PlasmaProps>(), {
  color: '#9EF2BE',
  speed: 1,
  direction: 'forward',
  scale: 1,
  opacity: 1,
  renderScale: 0.55,
  maxDpr: 1.5,
  targetFps: 60,
  iterations: 60
});

const ORIGINAL_ITERATIONS = 60;

const hexToRgb = (hex: string): [number, number, number] => {
  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
  if (!result) return [1, 0.5, 0.2];
  return [parseInt(result[1], 16) / 255, parseInt(result[2], 16) / 255, parseInt(result[3], 16) / 255];
};

const vertex = `#version 300 es
precision highp float;
in vec2 position;
in vec2 uv;
out vec2 vUv;
void main() {
  vUv = uv;
  gl_Position = vec4(position, 0.0, 1.0);
}
`;

const fragment = `#version 300 es
precision highp float;
uniform vec2 iResolution;
uniform float iTime;
uniform vec3 uCustomColor;
uniform float uUseCustomColor;
uniform float uSpeed;
uniform float uDirection;
uniform float uScale;
uniform float uOpacity;
uniform float uIterations;
uniform float uStepScale;
out vec4 fragColor;

void mainImage(out vec4 o, vec2 C) {
  vec2 center = iResolution.xy * 0.5;
  C = (C - center) / uScale + center;
  
  float i, d, z, T = iTime * uSpeed * uDirection;
  vec3 O, p, S;

  for (vec2 r = iResolution.xy, Q; ++i < 60.; O += o.w/d*o.xyz) {
    p = z*normalize(vec3(C-.5*r,r.y)); 
    p.z -= 4.; 
    S = p;
    d = p.y-T;
    
    p.x += .4*(1.+p.y)*sin(d + p.x*0.1)*cos(.34*d + p.x*0.05); 
    Q = p.xz *= mat2(cos(p.y+vec4(0,11,33,0)-T)); 
    z+= d = (abs(sqrt(length(Q*Q)) - .25*(5.+S.y))/3.+8e-4)*uStepScale; 
    o = 1.+sin(S.y+p.z*.5+S.z-length(S-p)+vec4(2,1,0,8));
    if (i >= uIterations) break;
  }
  
  o.xyz = tanh(O/1e4);
}

bool finite1(float x){ return !(isnan(x) || isinf(x)); }
vec3 sanitize(vec3 c){
  return vec3(
    finite1(c.r) ? c.r : 0.0,
    finite1(c.g) ? c.g : 0.0,
    finite1(c.b) ? c.b : 0.0
  );
}

void main() {
  vec4 o = vec4(0.0);
  mainImage(o, gl_FragCoord.xy);
  vec3 rgb = sanitize(o.rgb);
  
  float intensity = (rgb.r + rgb.g + rgb.b) / 3.0;
  vec3 customColor = intensity * uCustomColor;
  vec3 finalColor = mix(rgb, customColor, step(0.5, uUseCustomColor));
  
  float alpha = length(rgb) * uOpacity;
  fragColor = vec4(finalColor, alpha);
}`;

const containerRef = useTemplateRef('containerRef');

let cleanup: (() => void) | null = null;

const setup = () => {
  if (!containerRef.value) return;

  const container = containerRef.value;

  const prefersReducedMotion = window.matchMedia?.('(prefers-reduced-motion: reduce)').matches ?? false;

  const useCustomColor = props.color ? 1.0 : 0.0;
  const customColorRgb = props.color ? hexToRgb(props.color) : [1, 1, 1];

  const directionMultiplier = props.direction === 'reverse' ? -1.0 : 1.0;

  const renderScale = Math.min(Math.max(props.renderScale, 0.1), 1);
  const iterations = Math.round(Math.min(Math.max(props.iterations, 1), ORIGINAL_ITERATIONS));
  const frameInterval = props.targetFps > 0 ? 1000 / props.targetFps : 0;

  const renderer = new Renderer({
    webgl: 2,
    alpha: true,
    antialias: false,
    dpr: Math.min(window.devicePixelRatio || 1, Math.max(props.maxDpr, 0.1))
  });
  const gl = renderer.gl;
  const canvas = gl.canvas as HTMLCanvasElement;
  canvas.style.display = 'block';
  canvas.style.width = '100%';
  canvas.style.height = '100%';
  container.appendChild(canvas);

  const geometry = new Triangle(gl);

  const program = new Program(gl, {
    vertex: vertex,
    fragment: fragment,
    uniforms: {
      iTime: { value: 0 },
      iResolution: { value: new Float32Array([1, 1]) },
      uCustomColor: { value: new Float32Array(customColorRgb) },
      uUseCustomColor: { value: useCustomColor },
      uSpeed: { value: props.speed * 0.4 },
      uDirection: { value: directionMultiplier },
      uScale: { value: props.scale },
      uOpacity: { value: props.opacity },
      uIterations: { value: iterations },
      uStepScale: { value: ORIGINAL_ITERATIONS / iterations }
    }
  });

  const mesh = new Mesh(gl, { geometry, program });

  const setSize = () => {
    const rect = container.getBoundingClientRect();
    const width = Math.max(1, Math.floor(rect.width * renderScale));
    const height = Math.max(1, Math.floor(rect.height * renderScale));
    renderer.setSize(width, height);
    canvas.style.width = '100%';
    canvas.style.height = '100%';
    const res = program.uniforms.iResolution.value as Float32Array;
    res[0] = gl.drawingBufferWidth;
    res[1] = gl.drawingBufferHeight;
    if (prefersReducedMotion) renderer.render({ scene: mesh });
  };

  let resizePending = false;
  const ro = new ResizeObserver(() => {
    if (resizePending) return;
    resizePending = true;
    requestAnimationFrame(() => {
      resizePending = false;
      setSize();
    });
  });
  ro.observe(container);
  setSize();

  let raf = 0;
  let contextLost = false;
  let isVisible = true;
  let tabVisible = document.visibilityState !== 'hidden';
  let lastFrameTime = 0;
  let elapsedTime = 0;

  const loop = (t: number) => {
    if (contextLost || !isVisible || !tabVisible) {
      raf = 0;
      return;
    }

    raf = requestAnimationFrame(loop);

    // A one millisecond tolerance keeps the target frame rate from being halved by rAF jitter.
    if (frameInterval > 0 && lastFrameTime !== 0 && t - lastFrameTime < frameInterval - 1) return;

    // Time is accumulated instead of derived from the start, so pauses don't make the animation jump.
    elapsedTime += (lastFrameTime === 0 ? 0 : t - lastFrameTime) * 0.001;
    lastFrameTime = t;

    if (props.direction === 'pingpong') {
      const cycle = Math.sin(elapsedTime * 0.5) * directionMultiplier;
      (program.uniforms.uDirection as { value: number }).value = cycle;
    }

    (program.uniforms.iTime as { value: number }).value = elapsedTime;
    renderer.render({ scene: mesh });
  };

  const start = () => {
    if (raf || contextLost || !isVisible || !tabVisible || prefersReducedMotion) return;
    lastFrameTime = 0;
    raf = requestAnimationFrame(loop);
  };

  const stop = () => {
    if (!raf) return;
    cancelAnimationFrame(raf);
    raf = 0;
  };

  const handleContextLost = (e: Event) => {
    e.preventDefault();
    contextLost = true;
    stop();
  };

  const handleContextRestored = () => {
    contextLost = false;
    start();
  };

  canvas.addEventListener('webglcontextlost', handleContextLost);
  canvas.addEventListener('webglcontextrestored', handleContextRestored);

  const io = new IntersectionObserver(
    entries => {
      isVisible = entries.some(entry => entry.isIntersecting);
      if (isVisible) start();
      else stop();
    },
    { threshold: 0 }
  );
  io.observe(container);

  const handleVisibilityChange = () => {
    tabVisible = document.visibilityState !== 'hidden';
    if (tabVisible) start();
    else stop();
  };
  document.addEventListener('visibilitychange', handleVisibilityChange);

  if (!prefersReducedMotion) start();

  cleanup = () => {
    stop();
    ro.disconnect();
    io.disconnect();
    document.removeEventListener('visibilitychange', handleVisibilityChange);
    canvas.removeEventListener('webglcontextlost', handleContextLost);
    canvas.removeEventListener('webglcontextrestored', handleContextRestored);
    try {
      container.removeChild(canvas);
    } catch {}
    gl.getExtension('WEBGL_lose_context')?.loseContext();
  };
};

onMounted(() => {
  setup();
});

onBeforeUnmount(() => {
  cleanup?.();
});

watch(
  props,
  () => {
    cleanup?.();
    setup();
  },
  { deep: true }
);
</script>

<template>
  <div ref="containerRef" class="relative w-full h-full overflow-hidden" />
</template>
