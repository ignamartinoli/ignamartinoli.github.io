export function absoluteUrl(path?: string) {
  return path ? new URL(path, useSiteConfig().url).href : undefined
}
