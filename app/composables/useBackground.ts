// TEMPORARY: background comparison. To remove, delete app/components/bg/,
// BackgroundSwitcher.vue, SiteBackground.vue and this file, put <AppBackground />
// back in the layout, then `pnpm remove ogl`.
export const BACKGROUNDS = [
  'Original',
  'Aurora',
  'DotField',
  'Iridescence',
  'Plasma',
  'ShapeGrid'
] as const

export type Background = typeof BACKGROUNDS[number]

export const useBackground = () => useState<Background>('background', () => 'Original')
